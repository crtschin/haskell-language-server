{-# LANGUAGE TypeFamilies #-}
module Development.IDE.Core.Actions
( getAtPoint
, getDefinition
, getTypeDefinition
, getImplementationDefinition
, highlightAtPoint
, refsAtPoint
, workspaceSymbols
, lookupMod
) where

import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict                   as HM
import           Data.Maybe
import qualified Data.Text                             as T
import           Development.IDE.Core.API              (Query, Tracked,
                                                        fastForwardEach,
                                                        lastKnown, optionally,
                                                        peek, rewind, untrack)
import qualified Development.IDE.Core.API              as API
import           Development.IDE.Core.Internal.Tracked (untrackStale)
import           Development.IDE.Core.LookupMod        (lookupMod)
import           Development.IDE.Core.OfInterest
import           Development.IDE.Core.PluginUtils      (toCurrentLocation)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat            (DynFlags (..),
                                                        ms_hspp_opts)
import           Development.IDE.Graph
import qualified Development.IDE.Spans.AtPoint         as AtPoint
import           Development.IDE.Types.HscEnvEq        (hscEnv)
import           Development.IDE.Types.Location
import           GHC.Iface.Ext.Types                   (Identifier)
import qualified HieDb
import           Language.LSP.Protocol.Types           (DocumentHighlight (..),
                                                        SymbolInformation (..))

-- IMPORTANT NOTE : make sure all rules that these read with `peek` have a "Persistent Stale" rule defined,
-- so we can quickly answer as soon as the IDE is opened
-- Even if we don't have persistent information on disk for these rules, the persistent rule
-- should just return an empty result
-- It is imperative that the result of the persistent rule succeed in such a case, or we will
-- block waiting for the rule to be properly computed.

-- | Try to get hover text for the name under point.
getAtPoint :: NormalizedFilePath -> Position -> Query (Maybe (Maybe Range, [T.Text]))
getAtPoint file pos = runMaybeT $ do
  ide <- ask
  opts <- liftIO $ getIdeOptionsIO ide

  (hf, env, dflags, dkMap) <- MaybeT $ peek $ optionally $ (,,,)
    <$> lastKnown GetHieAst file
    <*> (hscEnv <$> untrack <$> lastKnown GhcSession file)
    <*> (untrack . fmap (ms_hspp_opts . msrModSummary) <$> lastKnown GetModSummary file)
    <*> (fromMaybe (DKMap mempty mempty mempty) <$> optionally (untrack <$> lastKnown GetDocMap file))
  shakeExtras <- lift askShake

  MaybeT $ liftIO $ AtPoint.atPoint opts shakeExtras hf dkMap env pos (extensionFlags dflags)

-- | The names at the position in the last known AST. They keep the spans of
-- that version, which 'toCurrentLocation' translates once they are locations.
symbolsAt
  :: Tracked HieAstResult -> Position -> (HieAstResult -> Position -> s) -> MaybeT Query s
symbolsAt ast pos f = hoistMaybe $ untrackStale <$> rewind f pos ast

lastKnownAst :: NormalizedFilePath -> MaybeT Query (Tracked HieAstResult)
lastKnownAst file = MaybeT $ peek $ optionally $ lastKnown GetHieAst file

-- | Goto Definition.
getDefinition :: NormalizedFilePath -> Position -> Query (Maybe [(Location, Identifier)])
getDefinition file pos = runMaybeT $ do
    ShakeExtras{ withHieDb, hiedbWriter } <- ask
    (ast, ImportMap imports) <- MaybeT $ peek $ optionally $
      (,) <$> lastKnown GetHieAst file <*> (untrack <$> lastKnown GetImportMap file)
    symbols <- symbolsAt ast pos AtPoint.definitionSymbols
    locationsWithIdentifier <- AtPoint.gotoDefinition withHieDb (lookupMod hiedbWriter) imports symbols
    traverse (\(location, identifier) -> (,identifier) <$> MaybeT (toCurrentLocation location))
      locationsWithIdentifier

getTypeDefinition :: NormalizedFilePath -> Position -> Query (Maybe [(Location, Identifier)])
getTypeDefinition file pos = runMaybeT $ do
    ShakeExtras{ withHieDb, hiedbWriter } <- ask
    ast <- lastKnownAst file
    symbols <- symbolsAt ast pos AtPoint.typeDefinitionSymbols
    locationsWithIdentifier <- AtPoint.gotoTypeDefinition withHieDb (lookupMod hiedbWriter) symbols
    traverse (\(location, identifier) -> (,identifier) <$> MaybeT (toCurrentLocation location))
      locationsWithIdentifier

getImplementationDefinition :: NormalizedFilePath -> Position -> Query (Maybe [Location])
getImplementationDefinition file pos = runMaybeT $ do
    ShakeExtras{ withHieDb, hiedbWriter } <- ask
    ast <- lastKnownAst file
    symbols <- symbolsAt ast pos AtPoint.implementationSymbols
    locs <- AtPoint.gotoImplementation withHieDb (lookupMod hiedbWriter) symbols
    traverse (MaybeT . toCurrentLocation) locs

highlightAtPoint :: NormalizedFilePath -> Position -> Query (Maybe [DocumentHighlight])
highlightAtPoint file pos = runMaybeT $ do
    ast <- lastKnownAst file
    hoistMaybe $ fastForwardEach <$> rewind AtPoint.documentHighlight pos ast

-- Refs are not an IDE action, so it is OK to be slow and (more) accurate
refsAtPoint :: NormalizedFilePath -> Position -> Action [Location]
refsAtPoint file pos = do
    ShakeExtras{withHieDb} <- getShakeExtras
    fs <- HM.keys <$> getFilesOfInterestUntracked
    asts <- HM.fromList . mapMaybe sequence . zip fs <$> API.use (traverse (optionally . lastKnown GetHieAst) fs)
    AtPoint.referencesAtPoint withHieDb file pos (AtPoint.FOIReferences asts)

workspaceSymbols :: T.Text -> Query (Maybe [SymbolInformation])
workspaceSymbols query = runMaybeT $ do
  ShakeExtras{withHieDb} <- ask
  res <- liftIO $ withHieDb (\hieDb -> HieDb.searchDef hieDb $ T.unpack query)
  pure $ mapMaybe AtPoint.defRowToSymbolInfo res
