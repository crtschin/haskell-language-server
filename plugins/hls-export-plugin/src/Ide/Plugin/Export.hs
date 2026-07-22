module Ide.Plugin.Export (descriptor, Log) where

import           Control.Applicative                          ((<|>))
import           Control.Lens
import           Control.Monad                                (filterM, when)
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

data Log = forall a. Pretty a => LogResolve a

instance Pretty Log where
  pretty (LogResolve msg) = pretty msg

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  let resolveRecorder = cmapWithPrio LogResolve recorder
      -- The explicit export action is more computationally heavy, so keep the
      -- provider cheap and move most of it into the resolve.
      explicitExportHandler = mkCodeActionHandlerWithResolve resolveRecorder explicitExportProvider explicitExportResolve
      exportHandlers = mkPluginHandler SMethod_TextDocumentCodeAction quickCodeActionHandlers <> explicitExportHandler
  in (defaultPluginDescriptor plId "Code actions for module export lists")
    { Ide.pluginHandlers = exportHandlers
    }

-- | Backs the @Export ...@ and @Unexport ...@ actions.
--
-- Follows the following general flow:
--   1. Locate what the cursor is currently highlighting.
--   2. Evaluate whether the target can be added or removed to the export list.
--   3. Emit the respective edits.
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
    --
    -- See Note [Reprinting erases CPP directives].
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
--   - a list holding a @module M@ re-export.
explicitExportProvider :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
explicitExportProvider state _plId (CodeActionParams _ _ doc range _) = do
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
            if exportListHasCpp msrc ps
              then pure False
              else do
                let modName = printOutputable $ moduleName $ ms_mod summ
                not <$> lift (isModuleExposed nfp modName)
      _ -> pure False
  pure . InL $
    [InR (mkAction "Export explicitly" & L.data_ ?~ toJSON ExportUsed) | offer]

-- | Resolve "Export explicitly".
--
-- Go over the module's reverse dependencies so hiedb sees every consumer, then
-- keep only the exports those consumers reference.
explicitExportResolve :: ResolveFunction IdeState ExportResolveData Method_CodeActionResolve
explicitExportResolve state _plId ca uri ExportUsed = do
  nfp <- getNormalizedFilePathE uri
  (ps, avails, typeEnv, msrc, modName) <- runActionE "Export.explicitly.resolve" state $ do
    pm <- useE GetParsedModuleWithComments nfp
    tmr <- useE TypeCheck nfp
    -- The usage query below only sees consumers hiedb has indexed. Index every
    -- reverse dep first, or still-referenced exports get trimmed.
    depInfo <- lift (useNoFile_ GetModuleGraph)
    let revDeps = fromMaybe [] (transitiveReverseDependencies nfp depInfo)

    -- Indexing only schedules the hiedb writes, so block until they land.
    _ <- usesE GetModIfaceFromDiskAndIndex revDeps
    liftIO $ awaitIndexed (shakeExtras state) revDeps
    msrc <- snd <$> useE GetFileContents nfp
    let modName = T.pack $ moduleNameString $ moduleName $ ms_mod (pm_mod_summary pm)
    let tcg = tmrTypechecked tmr
    pure (pm_parsed_source pm, tcg_exports tcg, tcg_type_env tcg, msrc, modName)

  -- Re-check we can do the action before touching the export list.
  exposed <- runIdeActionE "Export.explicitly.resolve.exposed" (shakeExtras state) $
    lift (isModuleExposed nfp modName)
  when exposed $ throwError $ PluginInternalError "Module is exposed publicly"
  let excludeFp = [fromNormalizedFilePath nfp]
      hieDb = withHieDb (shakeExtras state)
  used <- liftIO $ filterM (isReferencedExternally hieDb excludeFp) avails

  -- Emit alphabetically so the generated list is deterministic.
  let lexicalOrder = fmap (LexicalFastString . rdrNameFS) . ieParentName . unLoc
      isComplete parent members = case parentChildren typeEnv parent of
        Just children -> all (`elem` members) children
        Nothing       -> False
      desired = sortOn lexicalOrder (concatMap (availToLIE isComplete) used)
  case setExportList msrc ps desired of
    Just edits -> pure $ ca & L.edit ?~ singleFileEdit uri edits
    Nothing -> throwError $ PluginInternalError "Cannot rewrite the export list"
