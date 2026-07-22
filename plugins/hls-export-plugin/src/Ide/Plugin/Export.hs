{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Ide.Plugin.Export (descriptor, Log) where

import           Control.Applicative                           ((<|>))
import           Control.Concurrent.STM                        (atomically,
                                                                check, readTVar)
import           Control.Lens                                  hiding (use,
                                                                uses)
import           Control.Monad                                 (filterM, unless,
                                                                when)
import           Control.Monad.Error.Class                     (throwError)
import           Control.Monad.Except                          (ExceptT)
import           Control.Monad.IO.Class                        (liftIO)
import           Control.Monad.Trans.Class                     (lift)
import           Data.Aeson                                    (toJSON)
import qualified Data.HashMap.Strict                           as HM
import           Data.List                                     (sortOn)
import           Data.Maybe                                    (isJust,
                                                                isNothing)
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T
import           Data.Text.Utf16.Rope.Mixed                    (Rope)
import           Development.IDE
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping          (fromCurrentPosition)
import           Development.IDE.Core.Shake                    (HieDbWriter (..),
                                                                ShakeExtras (..),
                                                                getDiagnostics)
import qualified Development.IDE.Core.Shake                    as Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Error              (_TcRnUnusedTopBind,
                                                                msgEnvelopeErrorL)
import           Development.IDE.GHC.Compat.Util               (LexicalFastString (..))
import           Development.IDE.Graph.Classes                 (Hashable,
                                                                NFData)
import           Development.IDE.Import.DependencyInformation  (transitiveReverseDependencies)
import           Distribution.PackageDescription               (LibraryVisibility (..),
                                                                allLibraries,
                                                                exposedModules,
                                                                libVisibility)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Pretty                           (prettyShow)
import           GHC.Driver.Session                            (mainModuleNameIs)
import           GHC.Generics                                  (Generic)
import qualified GHC.LanguageExtensions.Type                   as LangExt (Extension (..))
import           Ide.Plugin.Cabal.Completion.Types             (ParseCabalFile (..))
import           Ide.Plugin.Cabal.Files                        (findCabalFileIn)
import           Ide.Plugin.Error
import           Ide.Plugin.Export.Cursor
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Exports
import           Ide.Plugin.Export.Utils
import           Ide.Plugin.Resolve                            (mkCodeActionWithResolveAndCommand)
import           Ide.Types
import qualified Ide.Types                                     as Ide
import qualified Language.LSP.Protocol.Lens                    as L
import           Language.LSP.Protocol.Message                 (Method (..),
                                                                SMethod (..))
import           Language.LSP.Protocol.Types
import           System.FilePath                               (takeDirectory)

data Log
  = forall a. Pretty a => LogResolve a
  | LogNoRemoveUnused NormalizedFilePath Text
  | LogShake Shake.Log

instance Pretty Log where
  pretty (LogResolve msg) = pretty msg
  pretty (LogNoRemoveUnused nfp why) =
    pretty ("No \"Remove unused exports\" action for " <> T.pack (fromNormalizedFilePath nfp) <> ": " <> why)
  pretty (LogShake msg) = pretty msg

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  let -- The edit is slow to compute, so compute it on resolve.
      (removeUnusedCommands, removeUnusedHandler) =
        mkCodeActionWithResolveAndCommand (cmapWithPrio LogResolve recorder) plId
          (removeUnusedProvider recorder) removeUnusedResolve
      exportHandlers = mkPluginHandler SMethod_TextDocumentCodeAction quickCodeActionHandlers <> removeUnusedHandler
  in (defaultPluginDescriptor plId "Code actions for module export lists")
    { Ide.pluginHandlers = exportHandlers
    , Ide.pluginCommands = removeUnusedCommands
    , Ide.pluginRules = closestCabalFileRule recorder
    }

data GetClosestCabalFile = GetClosestCabalFile
  deriving (Eq, Show, Generic)

instance Hashable GetClosestCabalFile
instance NFData GetClosestCabalFile

type instance RuleResult GetClosestCabalFile = NormalizedFilePath

closestCabalFileRule :: Recorder (WithPriority Log) -> Rules ()
closestCabalFileRule recorder =
  define (cmapWithPrio LogShake recorder) $ \GetClosestCabalFile dir -> do
    let path = fromNormalizedFilePath dir
        parent = takeDirectory path
    -- Rerun when the session restarts, e.g. after a cabal file is added.
    _ <- useNoFile_ GhcSessionIO
    here <- liftIO (findCabalFileIn path)
    case here of
      Just cabal -> pure ([], Just (toNormalizedFilePath' cabal))
      Nothing
        | parent == path -> pure ([], Nothing)
        | otherwise      -> ([],) <$> use GetClosestCabalFile (toNormalizedFilePath' parent)

quickCodeActionHandlers :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
quickCodeActionHandlers state _plId (CodeActionParams _ _ doc range _) = do
  let uri = doc ^. L.uri
  nfp <- getNormalizedFilePathE uri
  (ps, isCpp, mUnder, msrc) <- runActionE "Export.getInputs" state $ do
    pm <- useE GetParsedModuleWithComments nfp
    let ps = pm_parsed_source pm
        isCpp = xopt LangExt.Cpp (ms_hspp_opts (pm_mod_summary pm))
        mUnder = if isExplicit ps then locateUnderCursor (range ^. L.start) ps else Nothing
    -- Only a CPP module about to be offered an action needs the buffer (to find
    -- directives in the export list), so skip the fetch otherwise.
    msrc <- if isJust mUnder && isCpp then snd <$> useE GetFileContents nfp else pure Nothing
    pure (ps, isCpp, mUnder, msrc)
  case mUnder of
    -- A CPP module whose buffer we could not read may have directives in the
    -- export list that a reprint would silently erase. Withhold rather than risk
    -- it.
    Just under | not (isCpp && isNothing msrc) -> do
      -- The names GHC flags as defined-but-unused. Attach the action to the
      -- unused diagnostics as well.
      unusedDiags <- liftIO $ unusedTopBindDiagnostics state nfp
      pure . InL . map InR $
        [ ca
        | Just (verb, title, edits) <-
            [ addAction msrc under ps
            , removeAction msrc under ps
            ]
        , let fixes = [ d | d <- unusedDiags, locateUnderCursor (d ^. L.range . L.start) ps == Just under ]
              ca = mkAction (verb <> " `" <> title <> "`")
                     & L.edit ?~ singleFileEdit uri edits
                     & L.diagnostics .~ (if null fixes then Nothing else Just fixes)
        ]
    _ -> pure (InL [])

-- | The LSP diagnostics for names GHC reports as unused top-level definitions.
unusedTopBindDiagnostics :: IdeState -> NormalizedFilePath -> IO [Diagnostic]
unusedTopBindDiagnostics state nfp = do
  diags <- atomically $ getDiagnostics state
  pure [ fdLspDiagnostic d | d <- diags, fdFilePath d == nfp, isUnusedTopBind d ]
  where
    isUnusedTopBind =
      has (fdStructuredMessageL . _SomeStructuredMessage . msgEnvelopeErrorL . _TcRnUnusedTopBind)

addAction :: Maybe Rope -> UnderCursor -> ParsedSource -> Maybe (Text, Text, [TextEdit])
addAction msrc under ps = case under of
  Decl flavor n
    | n `isExported` ps -> Nothing
    | otherwise -> ("Export", T.pack (printRdrName n),) <$> addExport msrc ps (mkExportIE flavor n)
  Constructor t c
    | c `isExported` ps -> Nothing
    | otherwise ->
        ("Export", T.pack (printRdrName t) <> "(" <> T.pack (printRdrName c) <> ")",)
          <$> addConstructorExport msrc t c ps
  Header -> Nothing

removeAction :: Maybe Rope -> UnderCursor -> ParsedSource -> Maybe (Text, Text, [TextEdit])
removeAction msrc under ps = case under of
  Decl _ n -> ("Unexport", T.pack (printRdrName n),) <$> removeExport msrc ps n
  -- A bare uppercase entry denotes the type, so when the constructor shares the
  -- type's name, skip the standalone-removal fallback.
  Constructor t c ->
    ("Unexport", T.pack (printRdrName c),) <$>
      (removeConstructorExport msrc t c ps
        <|> if rdrNameFS c == rdrNameFS t then Nothing else removeExport msrc ps c)
  Header -> Nothing

-- See Note [All importing modules must be visible].
removeUnusedProvider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState Method_TextDocumentCodeAction
removeUnusedProvider recorder state _plId (CodeActionParams _ _ doc range _) = do
  nfp <- getNormalizedFilePathE (doc ^. L.uri)
  wholeProject <- isWholeProjectLoading state
  reason <-
    if not wholeProject
      then pure (Just "componentsLoading is not set to whole-project loading")
      else runIdeActionE "Export.removeUnused.offer" (shakeExtras state) $ do
        -- A stale parse is good enough for these checks.
        (pm, pmap) <- useWithStaleFastE GetParsedModuleWithComments nfp
        let ps = pm_parsed_source pm
            summ = pm_mod_summary pm
        case fromCurrentPosition pmap (range ^. L.start) >>= flip locateUnderCursor ps of
          Just Header -> do
            msrc <- if isCppModule summ
              then snd . fst <$> useWithStaleFastE GetFileContents nfp
              else pure Nothing
            exposed <- isExposed (\k f -> fmap fst <$> lift (useWithStaleFast k f)) nfp summ
            pure (reasonNotToTrim exposed summ msrc ps)
          _ -> pure (Just "the cursor is not on the module header")
  case reason of
    Nothing  -> pure (InL [InR (mkAction "Remove unused exports" & L.data_ ?~ toJSON ())])
    Just why -> InL [] <$ logWith recorder Debug (LogNoRemoveUnused nfp why)

{- Note [All importing modules must be visible]

"Remove unused exports" removes the exports that no module of the project uses.
Removing used exports is an easy way to break the working code, so we make our
lives easier and only emit the action when usage is easy to determine:
  - Whole project loading is turned on.
  - The module is not present in @exposed-modules@ of public libraries.
  - The module is not the main module of an executable.
  - Every module that imports this one compiles, so hiedb has its references.
  - No importing module uses CPP.
-}

-- See Note [All importing modules must be visible].
removeUnusedResolve :: ResolveFunction IdeState () Method_CodeActionResolve
removeUnusedResolve state _plId ca uri () = do
  nfp <- getNormalizedFilePathE uri
  isWholeProject <- isWholeProjectLoading state
  unless isWholeProject $ throwError PluginStaleResolve

  (ps, avails, msrc, revDeps) <-
    runActionE "Export.removeUnused.resolve" state $ do
      pm <- useE GetParsedModuleWithComments nfp
      tmr <- useE TypeCheck nfp
      depInfo <- lift (useNoFile_ GetModuleGraph)
      revDeps <-
        handleMaybe (PluginInternalError "This module is not in the loaded module graph")
          (transitiveReverseDependencies nfp depInfo)
      msrc <- snd <$> useE GetFileContents nfp
      let ps = pm_parsed_source pm
          summ = pm_mod_summary pm
      -- The offer used stale results, so check again on current ones.
      exposed <- lift (isExposed use nfp summ)
      when (isJust (reasonNotToTrim exposed summ msrc ps)) $ throwError PluginStaleResolve
      built <- lift (uses GetModIfaceFromDiskAndIndex revDeps)
      case [dep | (dep, Nothing) <- zip revDeps built] of
        [] -> pure ()
        broken@(worst : _) -> throwError . PluginInvalidUserState $
          T.pack (show (length broken))
            <> " module(s) importing this one do not build, starting with "
            <> T.pack (fromNormalizedFilePath worst)
      summs <- lift (uses GetModSummaryWithoutTimestamps revDeps)
      case [dep | (dep, Just s) <- zip revDeps summs, isCppModule (msrModSummary s)] of
        [] -> pure ()
        cpp : _ -> throwError . PluginInvalidUserState $
          "hiedb does not see the uses in inactive CPP branches of "
            <> T.pack (fromNormalizedFilePath cpp)
      pure (ps, tcg_exports (tmrTypechecked tmr), msrc, revDeps)

  -- hiedb writes finish after the rule returns, so wait for them.
  liftIO $ atomically $ do
    pending <- readTVar (indexPending (hiedbWriter (shakeExtras state)))
    check (not (any (`HM.member` pending) revDeps))

  usedNames <- liftIO $ filterM
    (isNameReferencedExternally (withHieDb (shakeExtras state)) [fromNormalizedFilePath nfp])
    (concatMap availNames avails)
  let wanted = keepConstructorsOfUsed (`elem` usedNames) avails
      used = filter (any wanted . availNames) avails
      refreshed
        | isExplicit ps = keepUsedExports msrc ps wanted avails
        | otherwise =
            addExportList ps (sortOn nameOrder (concatMap (availToLIE wanted) used))
  edits <- handleMaybe (PluginInternalError "Cannot rewrite the export list") refreshed

  pure $ ca & L.edit ?~ singleFileEdit uri edits

-- | Why "Remove unused exports" must not edit the module, if it must not.
--
-- See Note [All importing modules must be visible].
reasonNotToTrim :: Bool -> ModSummary -> Maybe Rope -> ParsedSource -> Maybe Text
reasonNotToTrim exposed summ msrc ps
  | hasCppOrReexport summ msrc ps =
      Just "the export list re-exports a module or holds a CPP directive"
  | exposed = Just "a public library of the package exposes the module"
  | moduleName (ms_mod summ) == mainModuleNameIs (ms_hspp_opts summ) =
      Just "the module is the main module of an executable"
  | otherwise = Nothing

-- | Whether a public library of the package lists the module in @exposed-modules@.
--
-- Note: Variable on rule execution. The provider permits stale results, the
-- resolve does not.
isExposed
  :: Monad m
  => (forall k v. IdeRule k v => k -> NormalizedFilePath -> m (Maybe v))
  -> NormalizedFilePath -> ModSummary -> m Bool
isExposed useRule nfp summ = do
  mCabal <- useRule GetClosestCabalFile (toNormalizedFilePath' (takeDirectory (fromNormalizedFilePath nfp)))
  mGpd <- maybe (pure Nothing) (useRule ParseCabalFile) mCabal
  pure (maybe True exposes mGpd)
  where
    name = moduleNameString (moduleName (ms_mod summ))
    exposes gpd = or
      [ name `elem` map prettyShow (exposedModules lib)
      | lib <- allLibraries (flattenPackageDescription gpd)
      , libVisibility lib == LibraryVisibilityPublic
      ]

isWholeProjectLoading :: IdeState -> ExceptT PluginError (HandlerM Config) Bool
isWholeProjectLoading state = do
  config <- runActionE "Export.clientConfig" state (lift getClientConfigAction)
  pure (componentsLoading config == PreferMultiWholeProjectLoading)

nameOrder :: GenLocated l (IE GhcPs) -> Maybe LexicalFastString
nameOrder = fmap (LexicalFastString . rdrNameFS) . ieParentName . unLoc
