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

import           Control.Monad.Extra                   (mapMaybeM)
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict                   as HM
import           Data.Maybe
import qualified Data.Text                             as T
import           Development.IDE.Core.API              (Aged, PositionMap,
                                                        Query, Tracked (..),
                                                        ageless, fastForward,
                                                        fastForwardEach,
                                                        recalls, rewind, settle,
                                                        settle_, untrack)
import           Development.IDE.Core.Internal.Tracked (unsafeMkStale,
                                                        unsafeUnAge)
import           Development.IDE.Core.LookupMod        (lookupMod)
import           Development.IDE.Core.OfInterest
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
                                                        SymbolInformation (..),
                                                        normalizedFilePathToUri,
                                                        uriToNormalizedFilePath)

-- IMPORTANT NOTE : make sure all rules `settle_`d by these have a "Persistent Stale" rule defined,
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

  hf <- settle_ GetHieAst file
  shakeExtras <- lift askShake

  env <- hscEnv . untrack <$> settle_ GhcSession file
  dflags <- untrack . fmap (ms_hspp_opts . msrModSummary) <$> settle_ GetModSummary file
  dkMap <- lift $ maybe (DKMap mempty mempty mempty) untrack <$> settle GetDocMap file

  MaybeT $ liftIO $ AtPoint.atPoint opts shakeExtras hf dkMap env pos (extensionFlags dflags)

-- | Converts locations in the source code to their current positions,
-- taking into account changes that may have occurred due to edits.
toCurrentLocation
  :: PositionMap s
  -> NormalizedFilePath
  -> Aged s Location
  -> Query (Maybe Location)
toCurrentLocation mapping file location =
  -- The Location we are going to might be in a different
  -- file than the one we are calling gotoDefinition from.
  -- So we check that the location file matches the file
  -- we are in.
  if nUri == normalizedFilePathToUri file
  -- The Location matches the file, so use the PositionMapping
  -- we have.
  then pure $ fastForward mapping location
  -- The Location does not match the file, so get the correct
  -- PositionMapping and use that instead.
  else runMaybeT $ do
      otherLocationFile <- hoistMaybe $ uriToNormalizedFilePath nUri
      Tracked _ otherLocationMapping <- settle_ GetHieAst otherLocationFile
      -- A location in another file comes from the hie file of that file, so
      -- assume that it has the age of the last known AST of that file.
      fastForward otherLocationMapping (unsafeMkStale (unsafeUnAge location))
  where
    nUri = toNormalizedUri $ ageless $ (\(Location uri _) -> uri) <$> location

-- | Goto Definition.
getDefinition :: NormalizedFilePath -> Position -> Query (Maybe [(Location, Identifier)])
getDefinition file pos = runMaybeT $ do
    ide@ShakeExtras{ withHieDb, hiedbWriter } <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    Tracked hf mapping <- settle_ GetHieAst file
    ImportMap imports <- untrack <$> settle_ GetImportMap file
    !pos' <- rewind mapping pos
    locationsWithIdentifier <- AtPoint.gotoDefinition withHieDb (lookupMod hiedbWriter) opts imports hf pos'
    mapMaybeM (\(location, identifier) -> do
      fixedLocation <- MaybeT $ toCurrentLocation mapping file location
      pure $ Just (fixedLocation, identifier)
      ) locationsWithIdentifier


getTypeDefinition :: NormalizedFilePath -> Position -> Query (Maybe [(Location, Identifier)])
getTypeDefinition file pos = runMaybeT $ do
    ide@ShakeExtras{ withHieDb, hiedbWriter } <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    Tracked hf mapping <- settle_ GetHieAst file
    !pos' <- rewind mapping pos
    locationsWithIdentifier <- AtPoint.gotoTypeDefinition withHieDb (lookupMod hiedbWriter) opts hf pos'
    mapMaybeM (\(location, identifier) -> do
      fixedLocation <- MaybeT $ toCurrentLocation mapping file location
      pure $ Just (fixedLocation, identifier)
      ) locationsWithIdentifier

getImplementationDefinition :: NormalizedFilePath -> Position -> Query (Maybe [Location])
getImplementationDefinition file pos = runMaybeT $ do
    ide@ShakeExtras{ withHieDb, hiedbWriter } <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    Tracked hf mapping <- settle_ GetHieAst file
    !pos' <- rewind mapping pos
    locs <- AtPoint.gotoImplementation withHieDb (lookupMod hiedbWriter) opts hf pos'
    traverse (MaybeT . toCurrentLocation mapping file) locs

highlightAtPoint :: NormalizedFilePath -> Position -> Query (Maybe [DocumentHighlight])
highlightAtPoint file pos = runMaybeT $ do
    Tracked hf mapping <- settle_ GetHieAst file
    !pos' <- rewind mapping pos
    pure $ fastForwardEach mapping $ AtPoint.documentHighlight hf pos'

-- Refs are not an IDE action, so it is OK to be slow and (more) accurate
refsAtPoint :: NormalizedFilePath -> Position -> Action [Location]
refsAtPoint file pos = do
    ShakeExtras{withHieDb} <- getShakeExtras
    fs <- HM.keys <$> getFilesOfInterestUntracked
    asts <- HM.fromList . mapMaybe sequence . zip fs <$> recalls GetHieAst fs
    AtPoint.referencesAtPoint withHieDb file pos (AtPoint.FOIReferences asts)

workspaceSymbols :: T.Text -> Query (Maybe [SymbolInformation])
workspaceSymbols query = runMaybeT $ do
  ShakeExtras{withHieDb} <- ask
  res <- liftIO $ withHieDb (\hieDb -> HieDb.searchDef hieDb $ T.unpack query)
  pure $ mapMaybe AtPoint.defRowToSymbolInfo res
