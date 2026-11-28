module Ide.Plugin.Export.RemoveUnused
  ( Log (..)
  , removeUnusedProvider
  , removeUnusedResolveProvider
  ) where

import           Control.Exception                             (IOException,
                                                                handle)
import           Control.Lens                                  hiding (use,
                                                                uses)
import           Control.Monad                                 (filterM, unless,
                                                                when)
import           Control.Monad.IO.Class                        (liftIO)
import           Control.Monad.Trans.Class                     (lift)
import           Control.Monad.Trans.Except                    (ExceptT,
                                                                mapExceptT,
                                                                runExceptT,
                                                                throwE,
                                                                withExceptT)
import           Data.Aeson                                    (toJSON)
import qualified Data.ByteString                               as BS
import           Data.Foldable                                 (for_)
import           Data.List                                     (partition,
                                                                sortOn)
import           Data.Maybe                                    (fromMaybe,
                                                                listToMaybe)
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T
import           Development.IDE
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping          (fromCurrentPosition)
import           Development.IDE.Core.Shake                    (ShakeExtras (..))
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util               (LexicalFastString (..))
import           Development.IDE.GHC.ExactPrint.CPP            (spanHasCpp)
import           Development.IDE.Import.DependencyInformation  (transitiveReverseDependencies)
import           Development.IDE.Types.Shake                   (WithHieDb)
import           Distribution.PackageDescription               (GenericPackageDescription,
                                                                LibraryVisibility (..),
                                                                allLibraries,
                                                                exposedModules,
                                                                libVisibility)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Pretty                           (prettyShow)
import           GHC.Driver.Session                            (mainModuleNameIs)
import           HieDb                                         (findReferences,
                                                                modInfoName,
                                                                modInfoUnit,
                                                                refIsGenerated,
                                                                (:.) (..))
import           Ide.Plugin.Cabal.Files                        (findClosestCabalFile)
import           Ide.Plugin.Cabal.Parse                        (parseCabalFileContents)
import           Ide.Plugin.Error
import           Ide.Plugin.Export.Cursor
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Exports
import           Ide.Plugin.Export.Utils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens                    as L
import           Language.LSP.Protocol.Message                 (Method (..))
import           Language.LSP.Protocol.Types

data Log
  = forall a. Pretty a => LogResolve a
  | LogNoRemoveUnused NormalizedFilePath Text

instance Pretty Log where
  pretty (LogResolve msg) = pretty msg
  pretty (LogNoRemoveUnused nfp why) =
    pretty ("No \"Remove unused exports\" action for " <> T.pack (fromNormalizedFilePath nfp) <> ": " <> why)

{- Note [All importing modules must be visible]

Removing used exports is an easy way to break the working code, so we err on the
safe side and only emit the action when usage is easy to determine:
  - Whole project loading is turned on.
  - The module is not present in @exposed-modules@ of public libraries.
  - The module is not the main module of an executable.
  - No importing module uses CPP.
-}

-- See Note [All importing modules must be visible].
removeUnusedProvider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState Method_TextDocumentCodeAction
removeUnusedProvider recorder state _plId (CodeActionParams _ _ doc range _) = do
  nfp <- getNormalizedFilePathE (doc ^. L.uri)
  -- A stale parse is good enough to decide whether to offer the action.
  onHeader <- runIdeActionE "Export.removeUnused.offer" (shakeExtras state) $ do
    (pm, pmap) <- useWithStaleFastE GetParsedModuleWithComments nfp
    if (fromCurrentPosition pmap (range ^. L.start) >>= flip locateUnderCursor (pm_parsed_source pm)) /= Just Header
      then pure Nothing
      else mkSource (snd . fst <$> useWithStaleFastE GetFileContents nfp) pm
  case onHeader of
    Nothing -> pure (InL [])
    Just src -> do
      wholeProject <- runActionE "Export.clientConfig" state (lift isWholeProjectLoading)
      verdict <- liftIO (runExceptT (checkTrimmable wholeProject nfp src))
      case verdict of
        Right () -> pure (InL [InR (action & L.data_ ?~ toJSON ())])
        Left why -> do
          logWith recorder Debug (LogNoRemoveUnused nfp why)
          caps <- lift pluginGetClientCapabilities
          pure (InL [InR (action & L.disabled ?~ CodeActionDisabled why) | supportsDisabledActions caps])
  where
    action = mkAction "Remove unused exports"

supportsDisabledActions :: ClientCapabilities -> Bool
supportsDisabledActions caps =
  caps ^? L.textDocument . _Just . L.codeAction . _Just . L.disabledSupport . _Just == Just True

-- See Note [All importing modules must be visible].
removeUnusedResolveProvider :: ResolveFunction IdeState () Method_CodeActionResolve
removeUnusedResolveProvider state _plId ca uri () = do
  nfp <- getNormalizedFilePathE uri
  (src, avails) <- runActionE "Export.removeUnused.resolve" state $ do
    wholeProject <- lift isWholeProjectLoading
    src <- handleMaybe PluginStaleResolve =<< getSource nfp
    -- The offer used stale results, so check again on current ones.
    withExceptT PluginInvalidUserState (mapExceptT liftIO (checkTrimmable wholeProject nfp src))
    tmr <- useE TypeCheck nfp
    depInfo <- lift (useNoFile_ GetModuleGraph)
    revDeps <-
      handleMaybe (PluginInternalError "This module is not in the loaded module graph")
        (transitiveReverseDependencies nfp depInfo)
    withExceptT PluginInvalidUserState (checkImporters revDeps)
    pure (src, tcg_exports (tmrTypechecked tmr))
  edits <- handleMaybe (PluginInternalError "Cannot rewrite the export list")
    =<< liftIO (trimmedExports src (withHieDb (shakeExtras state)) nfp avails)
  pure $ ca & L.edit ?~ singleFileEdit uri edits

checkTrimmable :: Bool -> NormalizedFilePath -> Source -> ExceptT Text IO ()
checkTrimmable wholeProject nfp src = do
  unless wholeProject $
    throwE "componentsLoading is not set to whole-project loading"
  when (hasCppOrReexport src) $
    throwE "the export list re-exports a module or holds a CPP directive"
  name <- maybe (throwE "the module has no header") (pure . unLoc) (hsmodName (unLoc src.parsed))
  when (name == mainModuleNameIs src.dflags) $
    throwE "the module is the main module of an executable"
  gpd <- maybe (throwE "no readable cabal file describes the module") pure
    =<< liftIO (readCabalFile nfp)
  when (any (exposes name) (allLibraries (flattenPackageDescription gpd))) $
    throwE "a public library of the package exposes the module"
  where
    exposes name lib = libVisibility lib == LibraryVisibilityPublic
      && moduleNameString name `elem` map prettyShow (exposedModules lib)

readCabalFile :: NormalizedFilePath -> IO (Maybe GenericPackageDescription)
readCabalFile nfp = handle (\(_ :: IOException) -> pure Nothing) $ do
  mCabal <- findClosestCabalFile (fromNormalizedFilePath nfp)
  contents <- traverse BS.readFile mCabal
  pure (either (const Nothing) Just . snd . parseCabalFileContents =<< contents)

checkImporters :: [NormalizedFilePath] -> ExceptT Text Action ()
checkImporters revDeps = do
  summs <- lift (uses GetModSummaryWithoutTimestamps revDeps)
  for_ (listToMaybe [dep | (dep, Just s) <- zip revDeps summs, isCppModule (msrModSummary s)]) $ \cpp ->
    throwE $ "hiedb does not see the uses in inactive CPP branches of " <> T.pack (fromNormalizedFilePath cpp)
  built <- lift (uses GetModIfaceFromDiskAndIndex revDeps)
  let broken = [dep | (dep, Nothing) <- zip revDeps built]
  for_ (listToMaybe broken) $ \worst ->
    throwE $ T.pack (show (length broken))
      <> " module(s) importing this one do not build, starting with "
      <> T.pack (fromNormalizedFilePath worst)

isWholeProjectLoading :: Action Bool
isWholeProjectLoading = (== PreferMultiWholeProjectLoading) . componentsLoading <$> getClientConfigAction

hasCppOrReexport :: Source -> Bool
hasCppOrReexport src = maybe headerHasCpp listHasCppOrReexport (hsmodExports modl)
  where
    modl = unLoc src.parsed
    headerHasCpp = maybe False (spanHasCpp src.buffer) $ do
      Range _ nameEnd <- srcSpanToRange . getLoc =<< hsmodName modl
      let bodyStart = case map getLocA (hsmodImports modl) ++ map getLocA (hsmodDecls modl) of
            sp : _ | Just (Range start _) <- srcSpanToRange sp -> start
            _ -> Position maxBound 0
      Just (Range nameEnd bodyStart)
    listHasCppOrReexport exports =
      any (isModuleContents . unLoc) (unLoc exports)
        || maybe False (spanHasCpp src.buffer) (srcSpanToRange (getLoc exports))
    isModuleContents IEModuleContents{} = True
    isModuleContents _                  = False

trimmedExports :: Source -> WithHieDb -> NormalizedFilePath -> [AvailInfo] -> IO (Maybe [TextEdit])
trimmedExports src hiedb nfp avails = do
  usedNames <- filterM
    (isNameReferencedExternally hiedb [fromNormalizedFilePath nfp])
    (concatMap availNames avails)
  let usedSet = mkNameSet usedNames
      wanted = keepConstructorsOfUsed (`elemNameSet` usedSet) avails
      (used, unused) = partition (any wanted . availNames) avails
  pure $
    if isExplicit src.parsed
      then keepUsedExports src wanted used unused
      else addExportList src.parsed (map (renderAvail wanted) (sortOn (LexicalFastString . getOccFS . availName) used))

-- An entry stays when another module uses any of its names.
keepUsedExports :: Source -> (Name -> Bool) -> [AvailInfo] -> [AvailInfo] -> Maybe [TextEdit]
keepUsedExports src wanted used unused =
  withExportList src (Just . dropChildren . dropEntries) Nothing
  where
    dropEntries l = orKeep (removeMatchingIE (\ie -> any (isEntryFor ie . availName) unused)) l
    dropChildren l = foldl dropChild l
      [ (parent, c) | avail <- used, let parent = availName avail
      , c <- availChildren avail, not (wanted c) ]
    dropChild l (parent, c) = orKeep (removeCtorUnderParent src.dflags (nameRdr parent) (nameRdr c)) l
    isEntryFor ie n = parentNameIs (getOccFS n) ie && isTypeIE ie == isTyConName n
    -- Disambiguate between type and pattern synonyms.
    isTypeIE IEVar{} = False
    isTypeIE _       = True
    orKeep f l = fromMaybe l (f l)

-- Importers can need a constructor of a used type, e.g. for standalone deriving
-- or coerce.
keepConstructorsOfUsed :: (Name -> Bool) -> [AvailInfo] -> Name -> Bool
keepConstructorsOfUsed used avails = \n -> used n || n `elemNameSet` constructors
  where
    constructors = mkNameSet
      [ c | avail <- avails, any used (availNames avail), c <- availNames avail, isDataConName c ]

renderAvail :: (Name -> Bool) -> AvailInfo -> Text
renderAvail wanted avail = case avail of
  AvailName n -> renderName n
  AvailTC parent _ _
    | not (null children), all wanted children -> renderExport ExportAll parentRdr
    | cs@(_ : _) <- filter wanted children -> renderTypeWith parentRdr (nameRdr <$> cs)
    | otherwise                            -> renderExport ExportAbs parentRdr
    where
      parentRdr = nameRdr parent
      children = availChildren avail
  AvailFL fl -> renderName (flSelector fl)
  where
    renderName n
      | isDataOcc (nameOccName n) = renderExport ExportPattern (nameRdr n)
      | otherwise                 = renderExport ExportName (nameRdr n)

availChildren :: AvailInfo -> [Name]
availChildren (AvailTC parent names pieces) = filter (/= parent) names ++ map flSelector pieces
availChildren _ = []

nameRdr :: Name -> RdrName
nameRdr = mkRdrUnqual . nameOccName

isNameReferencedExternally :: WithHieDb -> [FilePath] -> Name -> IO Bool
isNameReferencedExternally withDb exclude n = case nameModule_maybe n of
  Nothing  -> pure False
  Just mod -> do
    rows <- withDb $ \db ->
      findReferences db True (nameOccName n) (Just (moduleName mod)) (Just (moduleUnit mod)) exclude
    pure (any (external mod) rows)
  where
    -- Skip the uses that GHC inserts. See Note [Generated references] in the
    -- rename plugin.
    external mod row@(refRow :. _) =
      not (refIsGenerated refRow)
        && not (definedBy mod row)
    definedBy mod (_ :. info) =
      modInfoName info == moduleName mod && modInfoUnit info == moduleUnit mod
