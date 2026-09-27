{-# LANGUAGE PatternSynonyms #-}

-- | The rule API from before "Development.IDE.Core.Use" and
-- "Development.IDE.Core.Define", derived from their primitives.
--
-- Each name keeps its old type and its old behavior, including the failure
-- messages. "Development.IDE.Core.Shake" and
-- "Development.IDE.Core.PluginUtils" re-export these names. New code must use
-- the modules above.
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
  , IdeAction
  , pattern IdeAction
  , runIdeActionT
  , useWithStaleFast
  , useWithStaleFast'
  , FastResult (..)
  , runIdeAction
    -- * Plugin wrappers
  , useE
  , useMT
  , usesE
  , usesMT
  , useWithStaleE
  , useWithStaleMT
  , useWithStaleFastE
  , useWithStaleFastMT
  , runActionE
  , runActionMT
  , runIdeActionE
  , runIdeActionMT
  , uriToFilePathE
  , toCurrentPositionE
  , toCurrentPositionMT
  , fromCurrentPositionE
  , fromCurrentPositionMT
  , toCurrentRangeE
  , toCurrentRangeMT
  , fromCurrentRangeE
  , fromCurrentRangeMT
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

import           Control.Monad                        (join, void, when)
import           Control.Monad.IO.Class
import           Control.Monad.Reader                 (ReaderT)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
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
import qualified Data.Text                            as T
import           Development.IDE.Core.Internal        (DiagnosticSink (..),
                                                       IdeResult, IdeRule,
                                                       IdeState (shakeExtras),
                                                       Log (..), Query (..),
                                                       ShakeExtras (..),
                                                       defineRule,
                                                       getShakeExtras, hasRun,
                                                       mRunLspT,
                                                       mkDelayedAction,
                                                       shakeEnqueue, untracked)
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       fromCurrentPosition,
                                                       fromCurrentRange,
                                                       toCurrentPosition,
                                                       toCurrentRange,
                                                       zeroMapping)
import           Development.IDE.Core.RuleTypes       (GetClientSettings (..))
import           Development.IDE.Core.Use             (liftQuery, must, noFile,
                                                       one, recalls, request,
                                                       required, runQuery,
                                                       snapshot, use, use_,
                                                       uses, uses_)
import qualified Development.IDE.Core.Use             as Use
import           Development.IDE.Graph                (Action, Rules)
import           Development.IDE.Types.Location       (NormalizedFilePath,
                                                       fromNormalizedFilePath,
                                                       uriToFilePath')
import           Development.IDE.Types.Options        (IdeTesting (..))
import           Development.IDE.Types.Shake          (Value)
import           GHC.TypeLits                         (KnownSymbol)
import           Ide.Logger                           (Priority (Debug),
                                                       Recorder, WithPriority)
import           Ide.Plugin.Config                    (Config, PluginConfig,
                                                       parseConfig)
import           Ide.Plugin.Error                     (PluginError (..))
import qualified Ide.PluginUtils                      as HLS
import           Ide.Types                            (IdePlugins (..),
                                                       PluginDescriptor (..),
                                                       PluginId)
import qualified Language.LSP.Protocol.Message        as LSP
import           Language.LSP.Protocol.Types          (Position, Range, Uri)
import qualified Language.LSP.Server                  as LSP

------------------------------------------------------------------------------
-- Queries in rules

useNoFile :: IdeRule k v => k -> Action (Maybe v)
useNoFile k = use k noFile

useNoFile_ :: IdeRule k v => k -> Action v
useNoFile_ k = use_ k noFile

-- | Request a Rule result, it not available return the last computed result, if any, which may be stale
useWithStale :: IdeRule k v => k -> NormalizedFilePath -> Action (Maybe (v, PositionMapping))
useWithStale k f = fmap untracked <$> Use.recall k f

-- |Request a Rule result, it not available return the last computed result
--  which may be stale.
--
-- Throws an `BadDependency` exception which is caught by the rule system if
-- none available.
--
-- WARNING: Not suitable for PluginHandlers. Use `useWithStaleE` instead.
useWithStale_ :: IdeRule k v => k -> NormalizedFilePath -> Action (v, PositionMapping)
useWithStale_ k f = untracked <$> Use.recall_ k f

-- | Return the last computed result which might be stale.
usesWithStale
  :: (Traversable f, IdeRule k v)
  => k -> f NormalizedFilePath -> Action (f (Maybe (v, PositionMapping)))
usesWithStale k fs = fmap (fmap untracked) <$> recalls k fs

-- |Plural version of 'useWithStale_'
--
-- Throws an `BadDependency` exception which is caught by the rule system if
-- none available.
--
-- WARNING: Not suitable for PluginHandlers.
usesWithStale_
  :: (Traversable f, IdeRule k v)
  => k -> f NormalizedFilePath -> Action (f (v, PositionMapping))
usesWithStale_ k fs = fmap untracked <$> must recalls k fs

------------------------------------------------------------------------------
-- Queries in handlers

-- | IdeActions are used when we want to return a result immediately, even if it
-- is stale Useful for UI actions like hover, completion where we don't want to
-- block.
--
-- Run via 'runIdeAction'.
type IdeAction = Query

pattern IdeAction :: ReaderT ShakeExtras IO a -> IdeAction a
pattern IdeAction {runIdeActionT} = Query runIdeActionT
{-# COMPLETE IdeAction #-}

-- | A (maybe) stale result now, and an up to date one later
data FastResult a = FastResult
  { stale    :: Maybe (a, PositionMapping)
  , uptoDate :: IO (Maybe a)
  }

-- | Same as useWithStaleFast but lets you wait for an up to date result
useWithStaleFast' :: IdeRule k v => k -> NormalizedFilePath -> IdeAction (FastResult v)
useWithStaleFast' k f = do
  done <- fmap runIdentity <$> request k (Identity f)
  ran <- hasRun k f
  s <- fmap untracked <$> one snapshot k f
  case s of
    Nothing | not ran -> do
      a <- liftIO done
      pure (FastResult ((,zeroMapping) <$> a) (pure a))
    _ -> pure (FastResult s done)

-- | Lookup value in the database and return with the stale value immediately
-- Will queue an action to refresh the value.
-- Might block the first time the rule runs, but never blocks after that.
useWithStaleFast :: IdeRule k v => k -> NormalizedFilePath -> IdeAction (Maybe (v, PositionMapping))
useWithStaleFast k f = stale <$> useWithStaleFast' k f

runIdeAction :: String -> ShakeExtras -> IdeAction a -> IO a
runIdeAction _ = runQueryIn

------------------------------------------------------------------------------
-- Plugin wrappers

-- |ExceptT version of `use` that throws a PluginRuleFailed upon failure
useE :: IdeRule k v => k -> NormalizedFilePath -> ExceptT PluginError Action v
useE = Use.use_

-- |MaybeT version of `use`
useMT :: IdeRule k v => k -> NormalizedFilePath -> MaybeT Action v
useMT = Use.use_

-- |ExceptT version of `uses` that throws a PluginRuleFailed upon failure
usesE :: (Traversable f, IdeRule k v) => k -> f NormalizedFilePath -> ExceptT PluginError Action (f v)
usesE = Use.uses_

-- |MaybeT version of `uses`
usesMT :: (Traversable f, IdeRule k v) => k -> f NormalizedFilePath -> MaybeT Action (f v)
usesMT = Use.uses_

-- |ExceptT version of `useWithStale` that throws a PluginRuleFailed upon
-- failure
useWithStaleE :: IdeRule k v => k -> NormalizedFilePath -> ExceptT PluginError Action (v, PositionMapping)
useWithStaleE k f = untracked <$> Use.recall_ k f

-- |MaybeT version of `useWithStale`
useWithStaleMT :: IdeRule k v => k -> NormalizedFilePath -> MaybeT Action (v, PositionMapping)
useWithStaleMT k f = untracked <$> Use.recall_ k f

-- |ExceptT version of `useWithStaleFast` that throws a PluginRuleFailed upon
-- failure
useWithStaleFastE :: IdeRule k v => k -> NormalizedFilePath -> ExceptT PluginError IdeAction (v, PositionMapping)
useWithStaleFastE = one (must (\k -> liftQuery . fasts k))

-- |MaybeT version of `useWithStaleFast`
useWithStaleFastMT :: IdeRule k v => k -> NormalizedFilePath -> MaybeT IdeAction (v, PositionMapping)
useWithStaleFastMT = one (must (\k -> liftQuery . fasts k))

fasts
  :: (Traversable t, IdeRule k v)
  => k -> t NormalizedFilePath -> IdeAction (t (Maybe (v, PositionMapping)))
fasts k = traverse (useWithStaleFast k)

-- |ExceptT version of `runAction`, takes a ExceptT Action
runActionE :: MonadIO m => String -> IdeState -> ExceptT e Action a -> ExceptT e m a
runActionE herald ide = mapExceptT (liftIO . runQueued herald ide)

-- |MaybeT version of `runAction`, takes a MaybeT Action
runActionMT :: MonadIO m => String -> IdeState -> MaybeT Action a -> MaybeT m a
runActionMT herald ide = mapMaybeT (liftIO . runQueued herald ide)

-- | The same as 'Development.IDE.Core.Service.runAction', which this module
-- cannot import without an import cycle.
runQueued :: String -> IdeState -> Action a -> IO a
runQueued herald ide act = join $ shakeEnqueue (shakeExtras ide) (mkDelayedAction herald Debug act)

-- |ExceptT version of `runIdeAction`, takes a ExceptT IdeAction
runIdeActionE :: MonadIO m => String -> ShakeExtras -> ExceptT e IdeAction a -> ExceptT e m a
runIdeActionE _ extras = mapExceptT (liftIO . runQueryIn extras)

-- |MaybeT version of `runIdeAction`, takes a MaybeT IdeAction
runIdeActionMT :: MonadIO m => String -> ShakeExtras -> MaybeT IdeAction a -> MaybeT m a
runIdeActionMT _ extras = mapMaybeT (liftIO . runQueryIn extras)

runQueryIn :: ShakeExtras -> Query a -> IO a
runQueryIn extras q = runReaderT (runQueryT q) extras

-- |ExceptT version of `uriToFilePath` that throws a PluginInvalidParams upon
-- failure
uriToFilePathE :: Monad m => Uri -> ExceptT PluginError m FilePath
uriToFilePathE uri =
  required (PluginInvalidParams (T.pack $ "uriToFilePath' failed. Uri:" <> show uri)) (uriToFilePath' uri)

-- |ExceptT version of `toCurrentPosition` that throws a PluginInvalidUserState
-- upon failure
toCurrentPositionE :: Monad m => PositionMapping -> Position -> ExceptT PluginError m Position
toCurrentPositionE m = required (PluginInvalidUserState "toCurrentPosition") . toCurrentPosition m

-- |MaybeT version of `toCurrentPosition`
toCurrentPositionMT :: Monad m => PositionMapping -> Position -> MaybeT m Position
toCurrentPositionMT m = required (PluginInvalidUserState "toCurrentPosition") . toCurrentPosition m

-- |ExceptT version of `fromCurrentPosition` that throws a
-- PluginInvalidUserState upon failure
fromCurrentPositionE :: Monad m => PositionMapping -> Position -> ExceptT PluginError m Position
fromCurrentPositionE m = required (PluginInvalidUserState "fromCurrentPosition") . fromCurrentPosition m

-- |MaybeT version of `fromCurrentPosition`
fromCurrentPositionMT :: Monad m => PositionMapping -> Position -> MaybeT m Position
fromCurrentPositionMT m = required (PluginInvalidUserState "fromCurrentPosition") . fromCurrentPosition m

-- |ExceptT version of `toCurrentRange` that throws a PluginInvalidUserState
-- upon failure
toCurrentRangeE :: Monad m => PositionMapping -> Range -> ExceptT PluginError m Range
toCurrentRangeE m = required (PluginInvalidUserState "toCurrentRange") . toCurrentRange m

-- |MaybeT version of `toCurrentRange`
toCurrentRangeMT :: Monad m => PositionMapping -> Range -> MaybeT m Range
toCurrentRangeMT m = required (PluginInvalidUserState "toCurrentRange") . toCurrentRange m

-- |ExceptT version of `fromCurrentRange` that throws a PluginInvalidUserState
-- upon failure
fromCurrentRangeE :: Monad m => PositionMapping -> Range -> ExceptT PluginError m Range
fromCurrentRangeE m = required (PluginInvalidUserState "fromCurrentRange") . fromCurrentRange m

-- |MaybeT version of `fromCurrentRange`
fromCurrentRangeMT :: Monad m => PositionMapping -> Range -> MaybeT m Range
fromCurrentRangeMT m = required (PluginInvalidUserState "fromCurrentRange") . fromCurrentRange m

------------------------------------------------------------------------------
-- Configuration and test signals

-- See Note [Client configuration in Rules]
-- | Returns the client configuration, creating a build dependency.
--   You should always use this function when accessing client configuration
--   from build rules.
getClientConfigAction :: Action Config
getClientConfigAction = do
  ShakeExtras{lspEnv, idePlugins} <- getShakeExtras
  currentConfig <- (`LSP.runLspT` LSP.getConfig) `traverse` lspEnv
  mbVal <- unhashed <$> use_ GetClientSettings noFile
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
  ShakeExtras{ideTesting = IdeTesting testing, lspEnv} <- getShakeExtras
  kickSignal testing lspEnv files msgStart
  void $ uses rule files
  kickSignal testing lspEnv files msgEnd

------------------------------------------------------------------------------
-- Rule definitions

data RuleBody k v
  = Rule (k -> NormalizedFilePath -> Action (Maybe BS.ByteString, IdeResult v))
  | RuleNoDiagnostics (k -> NormalizedFilePath -> Action (Maybe BS.ByteString, Maybe v))
  | RuleWithCustomNewnessCheck
    { newnessCheck :: BS.ByteString -> BS.ByteString -> Bool
    , build :: k -> NormalizedFilePath -> Action (Maybe BS.ByteString, Maybe v)
    }
  | RuleWithOldValue (k -> NormalizedFilePath -> Value v -> Action (Maybe BS.ByteString, IdeResult v))

-- | Define a new Rule without early cutoff
define
  :: IdeRule k v
  => Recorder (WithPriority Log) -> (k -> NormalizedFilePath -> Action (IdeResult v)) -> Rules ()
define recorder op = defineEarlyCutoff recorder $ Rule $ \k f -> (Nothing,) <$> op k f

defineNoDiagnostics
  :: IdeRule k v
  => Recorder (WithPriority Log) -> (k -> NormalizedFilePath -> Action (Maybe v)) -> Rules ()
defineNoDiagnostics recorder op = defineEarlyCutoff recorder $ RuleNoDiagnostics $ \k f -> (Nothing,) <$> op k f

-- | Define a rule that can rerun without dirtying its dependents.
--
-- A rerun normally prompts every dependent to rerun. Early cutoff content
-- addresses the result, so hls-graph reruns dependents only when the returned
-- fingerprint has changed.
defineEarlyCutoff :: IdeRule k v => Recorder (WithPriority Log) -> RuleBody k v -> Rules ()
defineEarlyCutoff recorder = \case
  Rule op -> defineRule recorder Publish (==) $ \k f _ -> op k f
  RuleNoDiagnostics op ->
    defineRule recorder (LogAs (const LogDefineEarlyCutoffRuleNoDiagHasDiag)) (==) $ \k f _ ->
      second (mempty,) <$> op k f
  RuleWithCustomNewnessCheck{..} ->
    defineRule recorder (LogAs (const LogDefineEarlyCutoffRuleCustomNewnessHasDiag)) newnessCheck $ \k f _ ->
      second (mempty,) <$> build k f
  RuleWithOldValue op -> defineRule recorder Publish (==) op

defineNoFile :: IdeRule k v => Recorder (WithPriority Log) -> (k -> Action v) -> Rules ()
defineNoFile recorder f = defineNoDiagnostics recorder $ \k file ->
  if file == noFile
    then Just <$> f k
    else fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"

defineEarlyCutOffNoFile
  :: IdeRule k v => Recorder (WithPriority Log) -> (k -> Action (BS.ByteString, v)) -> Rules ()
defineEarlyCutOffNoFile recorder f = defineEarlyCutoff recorder $ RuleNoDiagnostics $ \k file ->
  if file == noFile
    then bimap Just Just <$> f k
    else fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"
