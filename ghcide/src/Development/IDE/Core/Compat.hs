{-# LANGUAGE PatternSynonyms #-}

module Development.IDE.Core.Compat
  ( -- * Queries in rules
    use
  , useNoFile
  , uses
  , use_
  , useNoFile_
  , uses_
  , useWithStale
  , usesWithStale
  , useWithStale_
  , usesWithStale_
    -- * Queries in handlers
  , useWithStaleFast
  , useWithStaleFast'
  , FastResult (..)
  , runIdeAction
    -- * Configuration and test signals
  , getClientConfigAction
  , getPluginConfigAction
  , runWithSignal
    -- * Rule definitions
  , RuleBody (..)
  , define
  , defineNoDiagnostics
  , defineEarlyCutoff
  , defineNoFile
  , defineEarlyCutOffNoFile
  ) where

import           Control.Concurrent.STM.Stats         (atomicallyNamed)
import           Control.Exception                    (throwIO)
import           Control.Monad                        (void, when)
import           Control.Monad.IO.Class
import           Control.Monad.Reader                 (runReaderT)
import           Data.Aeson                           (Result (Success), toJSON)
import qualified Data.Aeson.Types                     as A
import           Data.Bifunctor                       (second)
import qualified Data.ByteString.Char8                as BS
import           Data.Default                         (def)
import           Data.Foldable                        (find)
import           Data.Functor.Identity
import           Data.Hashable                        (unhashed)
import           Data.Maybe                           (fromMaybe)
import           Data.Proxy                           (Proxy)
import           Development.IDE.Core.Internal
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       zeroMapping)
import           Development.IDE.Core.RuleTypes       (GetClientSettings (..))
import           Development.IDE.Graph                (Action, Rules)
import           Development.IDE.Graph.Rule           (apply)
import           Development.IDE.Types.Location
import qualified Development.IDE.Types.Options        as Options
import           Development.IDE.Types.Shake          (A (..), Value,
                                                       currentValue)
import           GHC.Fingerprint                      (Fingerprint)
import           GHC.TypeLits                         (KnownSymbol)
import           Ide.Logger                           (Priority (Debug),
                                                       Recorder, WithPriority)
import           Ide.Plugin.Config                    (Config, PluginConfig,
                                                       parseConfig)
import qualified Ide.PluginUtils                      as HLS
import           Ide.Types                            (IdePlugins (..),
                                                       PluginDescriptor (..),
                                                       PluginId)
import qualified Language.LSP.Protocol.Message        as LSP
import qualified Language.LSP.Server                  as LSP

-- See Note [Client configuration in Rules]
-- | Returns the client configuration, creating a build dependency.
--   You should always use this function when accessing client configuration
--   from build rules.
getClientConfigAction :: Action Config
getClientConfigAction = do
  ShakeExtras{lspEnv, idePlugins} <- getShakeExtras
  currentConfig <- (`LSP.runLspT` LSP.getConfig) `traverse` lspEnv
  mbVal <- unhashed <$> useNoFile_ GetClientSettings
  let defValue = fromMaybe def currentConfig
  case A.parse (parseConfig idePlugins defValue) <$> mbVal of
    Just (Success c) -> return c
    _                -> return defValue

getPluginConfigAction :: PluginId -> Action PluginConfig
getPluginConfigAction plId = do
    config <- getClientConfigAction
    ShakeExtras{idePlugins = IdePlugins plugins} <- getShakeExtras
    let plugin = fromMaybe (error $ "Plugin not found: " <> show plId) $
                    find (\p -> pluginId p == plId) plugins
    return $ HLS.configForPlugin config plugin

-- | Return the most recent, potentially stale, value and a PositionMapping
-- for the version of that value.
lastValue :: IdeRule k v => k -> NormalizedFilePath -> Action (Maybe (v, PositionMapping))
lastValue key file = do
    s <- getShakeExtras
    liftIO $ lastValueIO s key file

-- | Define a new Rule without early cutoff
define
    :: IdeRule k v
    => Recorder (WithPriority Log) -> (k -> NormalizedFilePath -> Action (IdeResult v)) -> Rules ()
define recorder op = defineEarlyCutoff recorder $ Rule $ \k v -> (Nothing,) <$> op k v

defineNoDiagnostics
    :: IdeRule k v
    => Recorder (WithPriority Log) -> (k -> NormalizedFilePath -> Action (Maybe v)) -> Rules ()
defineNoDiagnostics recorder op = defineEarlyCutoff recorder $ RuleNoDiagnostics $ \k v -> (Nothing,) <$> op k v

-- | Request a Rule result if available
use :: IdeRule k v
    => k -> NormalizedFilePath -> Action (Maybe v)
use key file = runIdentity <$> uses key (Identity file)

-- | Request a Rule result, it not available return the last computed result, if any, which may be stale
useWithStale :: IdeRule k v
    => k -> NormalizedFilePath -> Action (Maybe (v, PositionMapping))
useWithStale key file = runIdentity <$> usesWithStale key (Identity file)

-- |Request a Rule result, it not available return the last computed result
--  which may be stale.
--
-- Throws an `BadDependency` exception which is caught by the rule system if
-- none available.
--
-- WARNING: Not suitable for PluginHandlers. Use `useWithStaleE` instead.
useWithStale_ :: IdeRule k v
    => k -> NormalizedFilePath -> Action (v, PositionMapping)
useWithStale_ key file = runIdentity <$> usesWithStale_ key (Identity file)

-- |Plural version of 'useWithStale_'
--
-- Throws an `BadDependency` exception which is caught by the rule system if
-- none available.
--
-- WARNING: Not suitable for PluginHandlers.
usesWithStale_ :: (Traversable f, IdeRule k v) => k -> f NormalizedFilePath -> Action (f (v, PositionMapping))
usesWithStale_ key files = do
    res <- usesWithStale key files
    case sequence res of
        Nothing -> liftIO $ throwIO $ BadDependency (show key)
        Just v  -> return v

runIdeAction :: String -> ShakeExtras -> IdeAction a -> IO a
runIdeAction _herald s i = runReaderT (runIdeActionT i) s

-- | A (maybe) stale result now, and an up to date one later
data FastResult a = FastResult { stale :: Maybe (a,PositionMapping), uptoDate :: IO (Maybe a)  }

-- | Lookup value in the database and return with the stale value immediately
-- Will queue an action to refresh the value.
-- Might block the first time the rule runs, but never blocks after that.
useWithStaleFast :: IdeRule k v => k -> NormalizedFilePath -> IdeAction (Maybe (v, PositionMapping))
useWithStaleFast key file = stale <$> useWithStaleFast' key file

-- | Same as useWithStaleFast but lets you wait for an up to date result
useWithStaleFast' :: IdeRule k v => k -> NormalizedFilePath -> IdeAction (FastResult v)
useWithStaleFast' key file = do
  -- This lookup directly looks up the key in the shake database and
  -- returns the last value that was computed for this key without
  -- checking freshness.

  -- Async trigger the key to be built anyway because we want to
  -- keep updating the value in the key.
  waitValue <- delayedAction $ mkDelayedAction ("C:" ++ show key ++ ":" ++ fromNormalizedFilePath file) Debug $ use key file

  s@ShakeExtras{state} <- askShake
  r <- liftIO $ atomicallyNamed "useStateFast" $ getValues state key file
  liftIO $ case r of
    -- block for the result if we haven't computed before
    Nothing -> do
      -- Check if we can get a stale value from disk
      res <- lastValueIO s key file
      case res of
        Nothing -> do
          a <- waitValue
          pure $ FastResult ((,zeroMapping) <$> a) (pure a)
        Just _ -> pure $ FastResult res waitValue
    -- Otherwise, use the computed value even if it's out of date.
    Just _ -> do
      res <- lastValueIO s key file
      pure $ FastResult res waitValue

useNoFile :: IdeRule k v => k -> Action (Maybe v)
useNoFile key = use key emptyFilePath

-- Requests a rule if available.
--
-- Throws an `BadDependency` exception which is caught by the rule system if
-- none available.
--
-- WARNING: Not suitable for PluginHandlers. Use `useE` instead.
use_ :: IdeRule k v => k -> NormalizedFilePath -> Action v
use_ key file = runIdentity <$> uses_ key (Identity file)

useNoFile_ :: IdeRule k v => k -> Action v
useNoFile_ key = use_ key emptyFilePath

-- |Plural version of `use_`
--
-- Throws an `BadDependency` exception which is caught by the rule system if
-- none available.
--
-- WARNING: Not suitable for PluginHandlers. Use `usesE` instead.
uses_ :: (Traversable f, IdeRule k v) => k -> f NormalizedFilePath -> Action (f v)
uses_ key files = do
    res <- uses key files
    case sequence res of
        Nothing -> liftIO $ throwIO $ BadDependency (show key)
        Just v  -> return v

-- | Plural version of 'use'
uses :: (Traversable f, IdeRule k v)
    => k -> f NormalizedFilePath -> Action (f (Maybe v))
uses key files = fmap (\(A value) -> currentValue value) <$> apply (fmap (Q . (key,)) files)

-- | Return the last computed result which might be stale.
usesWithStale :: (Traversable f, IdeRule k v)
    => k -> f NormalizedFilePath -> Action (f (Maybe (v, PositionMapping)))
usesWithStale key files = do
    _ <- apply (fmap (Q . (key,)) files)
    -- We don't look at the result of the 'apply' since 'lastValue' will
    -- return the most recent successfully computed value regardless of
    -- whether the rule succeeded or not.
    traverse (lastValue key) files

-- we use separate fingerprint rules to trigger the rebuild of the rule
useWithSeparateFingerprintRule
    :: (IdeRule k v, IdeRule k1 Fingerprint)
    => k1 -> k -> NormalizedFilePath -> Action (Maybe v)
useWithSeparateFingerprintRule fingerKey key file = do
    _ <- use fingerKey file
    useWithoutDependency key emptyFilePath

-- we use separate fingerprint rules to trigger the rebuild of the rule
useWithSeparateFingerprintRule_
    :: (IdeRule k v, IdeRule k1 Fingerprint)
    => k1 -> k -> NormalizedFilePath -> Action v
useWithSeparateFingerprintRule_ fingerKey key file = do
    useWithSeparateFingerprintRule fingerKey key file >>= \case
        Just v -> return v
        Nothing -> liftIO $ throwIO $ BadDependency (show key)

data RuleBody k v
  = Rule (k -> NormalizedFilePath -> Action (Maybe BS.ByteString, IdeResult v))
  | RuleNoDiagnostics (k -> NormalizedFilePath -> Action (Maybe BS.ByteString, Maybe v))
  | RuleWithCustomNewnessCheck
    { newnessCheck :: BS.ByteString -> BS.ByteString -> Bool
    , build :: k -> NormalizedFilePath -> Action (Maybe BS.ByteString, Maybe v)
    }
  | RuleWithOldValue (k -> NormalizedFilePath -> Value v -> Action (Maybe BS.ByteString, IdeResult v))

-- | Define a rule that can rerun without dirtying its dependents.
--
-- A rerun normally prompts every dependent to rerun. Early cutoff content
-- addresses the result, so hls-graph reruns dependents only when the returned
-- fingerprint has changed.
defineEarlyCutoff
    :: IdeRule k v
    => Recorder (WithPriority Log)
    -> RuleBody k v
    -> Rules ()
defineEarlyCutoff recorder = \case
    Rule op -> defineRule recorder Publish (==) $ \k f _ -> op k f
    RuleNoDiagnostics op -> defineRule recorder (LogAs (const LogDefineEarlyCutoffRuleNoDiagHasDiag)) (==) $ \k f _ -> second (mempty,) <$> op k f
    RuleWithCustomNewnessCheck{..} -> defineRule recorder (LogAs (const LogDefineEarlyCutoffRuleCustomNewnessHasDiag)) newnessCheck $ \k f _ -> second (mempty,) <$> build k f
    RuleWithOldValue op -> defineRule recorder Publish (==) op

defineNoFile :: IdeRule k v => Recorder (WithPriority Log) -> (k -> Action v) -> Rules ()
defineNoFile recorder f = defineNoDiagnostics recorder $ \k file -> do
    if file == emptyFilePath then do res <- f k; return (Just res) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"

defineEarlyCutOffNoFile :: IdeRule k v => Recorder (WithPriority Log) -> (k -> Action (BS.ByteString, v)) -> Rules ()
defineEarlyCutOffNoFile recorder f = defineEarlyCutoff recorder $ RuleNoDiagnostics $ \k file -> do
    if file == emptyFilePath then do (hashString, res) <- f k; return (Just hashString, Just res) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"


-- | sends a signal whenever shake session is run/restarted
-- being used in cabal and hlint plugin tests to know when its time
-- to look for file diagnostics
kickSignal :: KnownSymbol s => Bool -> Maybe (LSP.LanguageContextEnv c) -> [NormalizedFilePath] -> Proxy s -> Action ()
kickSignal testing lspEnv files msg = when testing $ liftIO $ mRunLspT lspEnv $
  LSP.sendNotification (LSP.SMethod_CustomMethod msg) $
  toJSON $ map fromNormalizedFilePath files

-- | Add kick start/done signal to rule
runWithSignal :: (KnownSymbol s0, KnownSymbol s1, IdeRule k v) => Proxy s0 -> Proxy s1 -> [NormalizedFilePath] -> k -> Action ()
runWithSignal msgStart msgEnd files rule = do
  ShakeExtras{ideTesting = Options.IdeTesting testing, lspEnv} <- getShakeExtras
  kickSignal testing lspEnv files msgStart
  void $ uses rule files
  kickSignal testing lspEnv files msgEnd
