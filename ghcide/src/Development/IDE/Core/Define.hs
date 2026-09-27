{-# LANGUAGE NoFieldSelectors #-}
-- | Rule definitions.
--
-- == Examples
--
-- A rule that publishes diagnostics:
--
-- @
-- docMapRule :: Rules ()
-- docMapRule = 'rule' $ \\GetDocMap file -> do
--   tmr <- use (current TypeCheck file)
--   ...
--   pure ('ok' docs)
-- @
--
-- Some rules don't emit diagnostics. This is tagged via the 'RuleDiagnostics'
-- typeclass. The key indicates whether its rule publishes diagnostics:
--
-- @
-- data GetModIface = GetModIface
--   deriving anyclass (Hashable, NFData, 'RuleDiagnostics' 'Quiet')
-- @
--
-- A rule without diagnostics, where
-- dependents only rerun when the fingerprint changes:
--
-- @
-- 'rule' $ \\GetModIface file ->
--   'cutoffBy' hirIfaceFp . 'output' \<$\> use (optionally (current GetModArtefacts file))
-- @
--
-- A rule of a key with 'Project' scope:
--
-- @
-- 'rule' $ \\GetModuleGraph 'Project' -> do
--   ...
-- @
--
-- Each file gets the whole module graph, but dependents rerun only when the
-- dependencies of that file change:
--
-- @
-- 'rule' $ \\GetModuleGraphTransDeps -> 'perFileCutoff' GetModuleGraph $ \\file graph ->
--   lookupFingerprint file graph (depTransDepsFingerprints graph)
-- @
module Development.IDE.Core.Define
  ( -- * Rules
    Body
  , rule
  , ruleWith
  , FingerprintCheck (..)
  , ruleWithPrevious
  , perFileCutoff
  , persistentRule
    -- * Outputs
  , Output
  , type Publishing (..)
  , RuleDiagnostics
  , Cutoff (..)
  , output
  , ok
  , failure
  , diagnostics
  , cutoff
  , cutoffBy
  ) where

import           Control.Lens                         (Lens', (&), (.~))
import qualified Data.ByteString.Char8                as BS
import           Data.Proxy                           (Proxy (..))
import qualified Data.Text                            as T
import           Development.IDE.Core.Internal        hiding (diagnostics)
import           Development.IDE.Core.Internal.Shake  (addPersistentRule,
                                                       getShakeExtrasRules)
import           Development.IDE.Core.Internal.Types  (KnownPublishing (..),
                                                       Publishing (..),
                                                       RuleDiagnostics)
import           Development.IDE.Core.PositionMapping (idDelta)
import           Development.IDE.Core.RuleTypes       (FileVersion)
import           Development.IDE.Core.Use             (MonadPluginFail (..),
                                                       use)
import           Development.IDE.GHC.Util
import           Development.IDE.Graph                hiding (Key)
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           Development.IDE.Types.Shake          hiding (Key)
import           GHC.Fingerprint
import           Ide.Plugin.Error                     (PluginError (..))

data Output (p :: Publishing) v = Output
  { result      :: Maybe v
  , diagnostics :: [FileDiagnostic]
  , cutoff      :: Cutoff
  }

data Cutoff
  = AlwaysRerun
  -- | Dependents rerun only when the fingerprint changes.
  --
  -- Use this when dependents are expensive, and a cheap fingerprint of the
  -- result exists, e.g. the hash of an interface file.
  | RerunOnChange BS.ByteString

-- | Dependents always rerun.
output :: Maybe v -> Output p v
output r = Output r [] AlwaysRerun

ok :: v -> Output p v
ok = output . Just

failure :: [FileDiagnostic] -> Output Publishes v
failure ds = Output Nothing ds AlwaysRerun

diagnostics :: Lens' (Output Publishes v) [FileDiagnostic]
diagnostics f o@Output{diagnostics = ds} = (\ds' -> o{diagnostics = ds'}) <$> f ds

cutoff :: Lens' (Output p v) Cutoff
cutoff f o@Output{cutoff = c} = (\c' -> o{cutoff = c'}) <$> f c

-- | Sets the 'cutoff' from the result.
--
-- Use this when the result has a cheap fingerprint, e.g. the hash of an
-- interface file.
cutoffBy :: (v -> BS.ByteString) -> Output p v -> Output p v
cutoffBy f o@Output{result} = o & cutoff .~ maybe AlwaysRerun (RerunOnChange . f) result

type Body k p v = k -> Scope k -> Action (Output p v)

-- | Wrapper for freshness-checking, takes the new and old fingerprints. If the
-- result is 'True', dependents aren't rerun.
newtype FingerprintCheck = FingerprintCheck (BS.ByteString -> BS.ByteString -> Bool)

rule :: (Key k, RuleResult k ~ v, RuleDiagnostics p k) => Body k p v -> Rules ()
rule = ruleWith (FingerprintCheck (==))

-- | Does the same as 'rule', with a custom check of the fingerprint.
--
-- Use this when some changes of the fingerprint must not rerun dependents.
ruleWith
  :: forall k v p. (Key k, RuleResult k ~ v, RuleDiagnostics p k)
  => FingerprintCheck -> Body k p v -> Rules ()
ruleWith unchanged body =
  registerRule (publishes (Proxy @p)) unchanged $ \k s _ -> body k s

-- | Does the same as 'rule', and gives the body the value of the previous run,
-- with the version of the file that it was built from.
--
-- Use this when the body can reuse the previous value, e.g. to skip reading an
-- unchanged file.
ruleWithPrevious
  :: forall k v p. (Key k, RuleResult k ~ v, RuleDiagnostics p k)
  => (Maybe (v, Maybe FileVersion) -> Body k p v) -> Rules ()
ruleWithPrevious body = registerRule (publishes (Proxy @p)) (FingerprintCheck (==)) $ \k s old ->
  body (previous old) k s
  where
    previous (Succeeded ver v) = Just (v, ver)
    previous (Stale _ ver v)   = Just (v, ver)
    previous (Failed _)        = Nothing

registerRule
  :: (Key k, RuleResult k ~ v)
  => Bool -> FingerprintCheck -> (k -> Scope k -> Value v -> Action (Output p v)) -> Rules ()
registerRule publishes (FingerprintCheck unchanged) body = do
  ShakeExtras{shakeRecorder} <- getShakeExtrasRules
  defineRule shakeRecorder publishes unchanged $ \k file old -> do
    s <- maybe (outOfScope k file) pure (fromFile file)
    Output{..} <- body k s old
    pure (fp cutoff, (diagnostics, result))
  where
    outOfScope k file = pluginFailed $ PluginInternalError $ T.pack $
      "Rule " ++ show k ++ " called with " ++ show file ++ ", outside its scope"
    fp AlwaysRerun       = Nothing
    fp (RerunOnChange f) = Just f

-- | Gives each file the value of the global key @base@. Dependents rerun only
-- when the fingerprint of their file changes.
--
-- Use this to give a large global value, e.g. the module graph, a separate
-- cutoff for each file.
perFileCutoff
  :: IdeRule base v
  => base -> (NormalizedFilePath -> v -> Maybe Fingerprint)
  -> NormalizedFilePath -> Action (Output p v)
perFileCutoff base fingerprintOf file = do
  graph <- use (currentAt base emptyFilePath)
  pure $ ok graph & cutoff .~ maybe AlwaysRerun (RerunOnChange . fingerprintToBS) (fingerprintOf file graph)

-- | Gives the key a value to read before its first build, e.g. an empty value
-- so a handler doesn't block. The value needs to be up-to-date with the current
-- version of the file.
persistentRule
  :: IdeRule k v => k -> (NormalizedFilePath -> Query (Maybe v)) -> Rules ()
persistentRule k getVal = addPersistentRule k $ fmap (fmap (,idDelta,Nothing)) . getVal
