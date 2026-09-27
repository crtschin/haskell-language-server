{-# LANGUAGE NoFieldSelectors #-}
-- | Rule definitions.
--
-- == Examples
--
-- A rule that publishes diagnostics:
--
-- @
-- docMapRule :: RuleScope ()
-- docMapRule = 'rule' $ \\GetDocMap file -> do
--   tmr <- 'use_' TypeCheck file
--   ...
--   pure ('ok' docs)
-- @
--
-- The key indicates whether its rule publishes diagnostics:
--
-- @
-- data GetModIface = GetModIface
--   deriving anyclass (Hashable, NFData, 'RuleDiagnostics' 'Quiet')
-- @
--
-- A rule without diagnostics, where dependents rerun only when the
-- fingerprint changes:
--
-- @
-- 'rule' $ \\GetModIface file ->
--   'cutoffBy' hirIfaceFp . 'output' \<$\> 'use' GetModArtefacts file
-- @
--
-- A custom check of the fingerprint:
--
-- @
-- 'ruleWith' ('FingerprintCheck' (<=)) $ \\NeedsCompilation file -> do
--   ...
-- @
--
-- A global rule:
--
-- @
-- 'rule' $ 'global' $ \\GetModuleGraph -> do
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
    RuleScope
  , withRuleRecorder
  , liftRules
  , Body
  , rule
  , ruleWith
  , FingerprintCheck (..)
  , ruleWithPrevious
  , global
  , perFileCutoff
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
  , fromIdeResult
  , cutoffOn
  , cutoffBy
  ) where

import           Control.Lens                      (Lens', (&), (.~))
import           Control.Monad.Reader
import qualified Data.Binary                       as B
import qualified Data.ByteString.Char8             as BS
import qualified Data.ByteString.Lazy              as LBS
import           Data.Proxy                        (Proxy (..))
import           Development.IDE.Core.Internal     hiding (diagnostics)
import           Development.IDE.Core.Internal.Publishing
import           Development.IDE.Core.RuleTypes    (FileVersion)
import           Development.IDE.Core.Use
import           Development.IDE.GHC.Util
import           Development.IDE.Graph
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           Development.IDE.Types.Shake
import           GHC.Fingerprint
import           Ide.Logger

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

-- | A result without diagnostics. Dependents always rerun.
output :: Maybe v -> Output p v
output r = Output r [] AlwaysRerun

ok :: v -> Output p v
ok = output . Just

failure :: [FileDiagnostic] -> Output Publishes v
failure ds = Output Nothing ds AlwaysRerun

-- | Only rules that publish can have diagnostics.
diagnostics :: Lens' (Output Publishes v) [FileDiagnostic]
diagnostics f o@Output{diagnostics = ds} = (\ds' -> o{diagnostics = ds'}) <$> f ds

cutoff :: Lens' (Output p v) Cutoff
cutoff f o@Output{cutoff = c} = (\c' -> o{cutoff = c'}) <$> f c

fromIdeResult :: IdeResult v -> Output Publishes v
fromIdeResult (ds, r) = output r & diagnostics .~ ds

-- | Sets the 'cutoff' from the result.
cutoffBy :: (v -> BS.ByteString) -> Output p v -> Output p v
cutoffBy f o@Output{result} = o & cutoff .~ maybe AlwaysRerun (RerunOnChange . f) result

-- | Dependents rerun only when the encoding of the value changes.
cutoffOn :: B.Binary a => a -> Cutoff
cutoffOn = RerunOnChange . LBS.toStrict . B.encode

-- | Defines rules.
newtype RuleScope a = RuleScope (ReaderT (Recorder (WithPriority Log)) Rules a)
  deriving (Functor, Applicative, Monad, MonadIO)

withRuleRecorder :: Recorder (WithPriority Log) -> RuleScope a -> Rules a
withRuleRecorder recorder (RuleScope m) = runReaderT m recorder

-- | For 'Rules' operations with no 'RuleScope' version, e.g. 'addIdeGlobal'.
liftRules :: Rules a -> RuleScope a
liftRules = RuleScope . lift

type Body k p v = k -> NormalizedFilePath -> Action (Output p v)

-- | Wrapper for freshness-checking, takes the new and old fingerprints. If the
-- result is 'True', dependents aren't reran.
newtype FingerprintCheck = FingerprintCheck (BS.ByteString -> BS.ByteString -> Bool)

-- | Defines the rule of a key. The 'RuleDiagnostics' instance of the key sets
-- whether the rule publishes diagnostics.
rule :: (IdeRule k v, RuleDiagnostics p k) => Body k p v -> RuleScope ()
rule = ruleWith (FingerprintCheck (==))

-- | Does the same as 'rule', with a custom check of the fingerprint.
--
-- Use this when some changes of the fingerprint must not rerun dependents.
ruleWith
  :: forall k v p. (IdeRule k v, RuleDiagnostics p k)
  => FingerprintCheck -> Body k p v -> RuleScope ()
ruleWith unchanged body =
  registerRule (publishes (Proxy @p)) unchanged $ \k file _ -> body k file

-- | Does the same as 'rule', and gives the body the value of the previous run,
-- with the version of the file that it was built from.
--
-- Use this when the body can reuse the previous value, e.g. to skip reading an
-- unchanged file.
ruleWithPrevious
  :: forall k v p. (IdeRule k v, RuleDiagnostics p k)
  => (k -> NormalizedFilePath -> Maybe (v, Maybe FileVersion) -> Action (Output p v))
  -> RuleScope ()
ruleWithPrevious body = registerRule (publishes (Proxy @p)) (FingerprintCheck (==)) $ \k file old ->
  body k file $ case old of
    Succeeded ver v -> Just (v, ver)
    Stale _ ver v   -> Just (v, ver)
    Failed _        -> Nothing

registerRule
  :: IdeRule k v
  => Bool -> FingerprintCheck -> (k -> NormalizedFilePath -> Value v -> Action (Output p v)) -> RuleScope ()
registerRule publishes (FingerprintCheck unchanged) body = RuleScope $ ReaderT $ \recorder ->
  defineRule recorder sink unchanged $ \k file old -> do
    Output{..} <- body k file old
    pure (fp cutoff, (diagnostics, result))
  where
    sink
      | publishes = Publish
      | otherwise = LogAs LogRuleDoesNotPublishDiagnostics
    fp AlwaysRerun       = Nothing
    fp (RerunOnChange f) = Just f

-- | The body of a rule that has one value for the whole program. Read the value
-- with 'noFile'.
--
-- Use this for keys that aren't associated with a single file, e.g. the module graph.
global :: Show k => (k -> Action (Output p v)) -> Body k p v
global body k file
  | file == noFile = body k
  | otherwise = fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"

-- | Gives each file the value of the global key @base@, with a fingerprint for
-- that file. Dependents rerun only when the fingerprint of their file changes.
--
-- Use this to give a large global value, e.g. the module graph, a separate
-- cutoff for each file.
perFileCutoff
  :: IdeRule base v
  => base -> (NormalizedFilePath -> v -> Maybe Fingerprint)
  -> NormalizedFilePath -> Action (Output p v)
perFileCutoff base fingerprintOf file = do
  graph <- use_ base noFile
  pure $ ok graph & cutoff .~ maybe AlwaysRerun (RerunOnChange . fingerprintToBS) (fingerprintOf file graph)
