module Ide.Plugin.Export (descriptor, Log) where

import           Control.Applicative                          ((<|>))
import           Control.Lens
import           Control.Monad                                (filterM, unless,
                                                               when)
import           Control.Monad.Error.Class                    (throwError)
import           Control.Monad.IO.Class                       (liftIO)
import           Control.Monad.Trans.Class                    (lift)
import           Data.Aeson                                   (toJSON)
import           Data.List                                    (sortOn)
import           Data.Maybe                                   (fromMaybe,
                                                               isJust,
                                                               isNothing)
import           Data.Text                                    (Text)
import qualified Data.Text                                    as T
import           Data.Text.Utf16.Rope.Mixed                   (Rope)
import           Development.IDE
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake                   (ShakeExtras (..))
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.Import.DependencyInformation
import qualified GHC.LanguageExtensions.Type                  as LangExt (Extension (..))
import           Ide.Plugin.Error
import           Ide.Plugin.Export.Cursor
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Exports
import           Ide.Plugin.Export.Utils
import           Ide.Plugin.Resolve
import           Ide.Types
import qualified Ide.Types                                    as Ide
import qualified Language.LSP.Protocol.Lens                   as L
import           Language.LSP.Protocol.Message                (Method (..),
                                                               SMethod (..))
import           Language.LSP.Protocol.Types

data Log
  = -- | Any sub-component's log, such as the resolve helper's.
    forall a. Pretty a => LogSub a

instance Pretty Log where
  pretty (LogSub msg) = pretty msg

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  let resolveRecorder = cmapWithPrio LogSub recorder
      -- Resolving is heavy, so a client without resolve support gets a command
      -- to invoke rather than the work running inline on every request.
      (explicitExportCommands, explicitExportHandler) =
        mkCodeActionWithResolveAndCommand resolveRecorder plId
          (explicitExportProvider recorder) explicitExportResolve
      exportHandlers = mkPluginHandler SMethod_TextDocumentCodeAction quickCodeActionHandlers <> explicitExportHandler
  in (defaultPluginDescriptor plId "Code actions for module export lists")
    { Ide.pluginHandlers = exportHandlers
    , Ide.pluginCommands = explicitExportCommands
    }

-- | Backs the @Export ...@ and @Unexport ...@ actions.
quickCodeActionHandlers :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
quickCodeActionHandlers state _plId (CodeActionParams _ _ doc range _) = do
  let uri = doc ^. L.uri
  nfp <- getNormalizedFilePathE uri
  (ps, isCpp, mUnder, msrc) <- runActionE "Export.getInputs" state $ do
    pm <- useE GetParsedModuleWithComments nfp
    let ps = pm_parsed_source pm
        isCpp = xopt LangExt.Cpp (ms_hspp_opts (pm_mod_summary pm))
        mUnder = if isExplicit ps then locateUnderCursor (range ^. L.start) ps else Nothing
    -- Only a CPP module would need the buffer, skip the fetch otherwise.
    msrc <- if isJust mUnder && isCpp then snd <$> useE GetFileContents nfp else pure Nothing
    pure (ps, isCpp, mUnder, msrc)
  case mUnder of
    -- Without the buffer we cannot tell whether a CPP module's export list holds
    -- a directive, so skip rather than risk erasing one.
    Just under | not (isCpp && isNothing msrc) -> do
      -- Also attach the action to any unused-binding diagnostics it fixes.
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

-- | Offer "Export explicitly" when the cursor is on the module header. The
-- action is withheld on:
--   - library-exposed modules
--   - an export list whose span holds a CPP directive
--   - a list holding a @module M@ re-export
--   - a project with a component the session has not loaded, per
--     Note [Every consumer must be visible].
explicitExportProvider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState Method_TextDocumentCodeAction
explicitExportProvider _recorder state _plId (CodeActionParams _ _ doc range _) = do
  nfp <- getNormalizedFilePathE (doc ^. L.uri)
  offer <- runIdeActionE "Export.explicitly.offer" (shakeExtras state) $ do
    -- fine to use stale here, as we only need to know if we're in the header.
    (pm, pmap) <- useWithStaleFastE GetParsedModuleWithComments nfp
    let ps = pm_parsed_source pm
        summ = pm_mod_summary pm
        isCpp = xopt LangExt.Cpp (ms_hspp_opts summ)
        -- map the cursor back onto the stale tree we actually read
        stalePosition = fromCurrentPosition pmap (range ^. L.start) >>= flip locateUnderCursor ps
    case stalePosition of
      Just Header
        | hasModuleReexport ps -> pure False
        | otherwise -> do
            -- Fetched only to check for a directive in the export-list span.
            msrc <- if isCpp then snd . fst <$> useWithStaleFastE GetFileContents nfp else pure Nothing
            -- Without the buffer a CPP module's list reads as directive-free,
            -- so withhold rather than trust it.
            if (isCpp && isNothing msrc) || exportListHasCpp msrc ps
              then pure False
              else do
                let modName = printOutputable $ moduleName $ ms_mod summ
                (exposed, declared) <- lift (exposureCheck nfp modName)
                if exposed then pure False else do
                  -- See Note [Every consumer must be visible].
                  depInfo <- useWithStaleFastE GetModuleGraph emptyFilePath
                  let unloaded = unloadedOf (fst depInfo) declared
                  pure (null unloaded)
      _ -> pure False
  pure . InL $
    [InR (mkAction "Export explicitly" & L.data_ ?~ toJSON ExportUsed) | offer]

{- Note [Every consumer must be visible]

An unused export is only ever an absence of references, so we need to ensure
that we make an attempt at finding dependents.
  - An unloaded component contributes no reverse dependencies and no reference
    rows. 'unloadedOf' compares what the owning package's cabal file declares
    against what the session knows.
  - A stale index can also obscure active references, so 'isIndexCurrent' checks
    each reverse dependency against its @.hie@ file before the query runs.
-}

-- | Resolve "Export explicitly".
explicitExportResolve :: ResolveFunction IdeState ExportResolveData Method_CodeActionResolve
explicitExportResolve state _plId ca uri ExportUsed = do
  nfp <- getNormalizedFilePathE uri
  (ps, avails, msrc, modName, revDeps) <-
    runActionE "Export.explicitly.resolve" state $ do
      pm <- useE GetParsedModuleWithComments nfp
      tmr <- useE TypeCheck nfp
      depInfo <- lift (useNoFile_ GetModuleGraph)
      revDeps <-
        maybe (throwError (PluginInternalError "This module is not in the loaded module graph")) pure
          (transitiveReverseDependencies nfp depInfo)
      msrc <- snd <$> useE GetFileContents nfp
      let modName = T.pack $ moduleNameString $ moduleName $ ms_mod (pm_mod_summary pm)
      pure (pm_parsed_source pm, tcg_exports (tmrTypechecked tmr), msrc, modName, revDeps)

  -- The provider decided on a stale tree, so re-establish its conditions here.
  -- CPP is the third, handled inside 'retainExports'.
  when (hasModuleReexport ps) $
    throwError $ PluginInternalError "The export list re-exports a whole module"

  exposed <- runIdeActionE "Export.explicitly.resolve.cabal" (shakeExtras state) $
    lift (isModuleExposed nfp modName)
  when exposed $
    throwError $ PluginInternalError "Module is exposed publicly"

  -- See Note [Every consumer must be visible].
  stale <- liftIO $ filterM (fmap not . isIndexCurrent hieDb) revDeps
  unless (null stale) $
    throwError $ PluginInternalError $
      "Cannot tell which exports are used: these reverse dependencies are not \
      \indexed yet. " <> summarise (map (T.pack . fromNormalizedFilePath) stale)

  used <- liftIO $ filterM (isReferencedExternally hieDb [fromNormalizedFilePath nfp]) avails
  when (null used) $
    throwError $ PluginInternalError
      "No export is referenced from outside this module, and emptying the \
      \export list is never the intended edit"

  edits <- case hsmodExports (unLoc ps) of
    -- An implicit list has no author formatting to preserve, so generate it.
    Nothing ->
      maybe (throwError (PluginInternalError "Cannot rewrite the export list")) pure
        (addExportList ps (sortOn lexicalOrder (concatMap availToLIE used)))
    -- Nothing matched means every entry survives, so the action is a no-op.
    Just _ -> pure $ fromMaybe [] (retainExports msrc ps (concatMap keepNames used))
  pure $ ca & L.edit ?~ singleFileEdit uri edits
  where
    hieDb = withHieDb (shakeExtras state)

    -- Head plus members, so an entry survives when any name it brings into
    -- scope is used.
    keepNames a = map (occNameFS . nameOccName) (availName a : availNames a)

    -- Generated lists are emitted alphabetically so the output is stable.
    lexicalOrder = fmap (LexicalFastString . rdrNameFS) . ieParentName . unLoc

    summarise xs = T.intercalate ", " (take 5 xs)
                <> if length xs > 5 then ", and " <> T.pack (show (length xs - 5)) <> " more" else ""
