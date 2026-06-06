{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Export (descriptor) where

import           Control.Applicative                          ((<|>))
import           Control.Concurrent.STM                       (atomically)
import           Control.Lens
import           Control.Monad                                (filterM)
import           Control.Monad.Error.Class                    (throwError)
import           Control.Monad.IO.Class                       (liftIO)
import           Control.Monad.Trans                          (lift)
import           Data.Aeson                                   (toJSON)
import           Data.Maybe                                   (fromMaybe,
                                                               mapMaybe)
import           Data.Text                                    (Text)
import qualified Data.Text                                    as T
import           Development.IDE
import           Development.IDE.Core.PluginUtils             (runActionE, useE,
                                                               usesE)
import           Development.IDE.Core.Shake                   (ShakeExtras (..),
                                                               getDiagnostics)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Error             (_TcRnMessage,
                                                               msgEnvelopeErrorL)
import           Development.IDE.Import.DependencyInformation (transitiveReverseDependencies)
import           GHC.Tc.Errors.Types                          (TcRnMessage (TcRnUnusedName),
                                                               UnusedNameProv (UnusedNameTopDecl))
import           Ide.Plugin.Error                             (PluginError (..),
                                                               getNormalizedFilePathE)
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

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = do
  let (explicitExportCommand, explicitExportHandler) = mkCodeActionWithResolveAndCommand mempty plId explicitExportCodeActionProvider explicitExportCodeActionResolveProvider
      exportHandlers = mkPluginHandler SMethod_TextDocumentCodeAction quickCodeActionHandlers
  (defaultPluginDescriptor plId "Code actions for module export lists")
    { Ide.pluginHandlers = explicitExportHandler <> exportHandlers
    , Ide.pluginCommands = explicitExportCommand
    }

explicitExportCodeActionProvider :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
explicitExportCodeActionProvider state _pId (CodeActionParams _ _ doc range _) = do
  nfp <- getNormalizedFilePathE (doc ^. L.uri)
  (offerUsed, offerAll) <-
    runActionE "Export.GetParsedModuleWithComments" state $ do
      pm <- useE GetParsedModuleWithComments nfp
      let ps = pm_parsed_source pm
      case locateUnderCursor (range ^. L.start) ps of
        Just Header -> do
          -- Trimming is withheld on public API: hiedb cannot see external
          -- consumers, so dropping an exposed export is unsafe. Listing
          -- every symbol only ever adds exports, so doesn't need the same
          -- restriction.
          let modName = T.pack $ moduleNameString $ moduleName $ ms_mod $ pm_mod_summary pm
          trimSafe <- not <$> lift (isModuleExposed nfp modName)
          let hasUnexported = not (all (flip isExported ps . snd) (moduleExports ps))
          pure (trimSafe, hasUnexported)
        _ -> pure (False, False)
  pure . InL . map InR $
    [ mkAction "Export explicitly" & L.data_ ?~ toJSON ExportUsed | offerUsed ]
      ++ [ mkAction "Export all symbols" & L.data_ ?~ toJSON ExportEverything | offerAll ]

explicitExportCodeActionResolveProvider :: ResolveFunction IdeState ExportMode 'Method_CodeActionResolve
explicitExportCodeActionResolveProvider state _pId ca uri mode = do
  nfp <- getNormalizedFilePathE uri
  medits <- case mode of
    ExportEverything ->
      runActionE "Export.ResolveAll" state $ do
        pm <- useE GetParsedModuleWithComments nfp
        let ps = pm_parsed_source pm
        pure $ setExportListExpanding ps (map (uncurry mkExportIE) (moduleExports ps))
    ExportUsed -> do
      (pm, tmr) <-
        runActionE "Export.Resolve" state $ do
          pm <- useE GetParsedModuleWithComments nfp
          tmr <- useE TypeCheck nfp
          -- Index reverse deps first, otherwise the usage query misses consumers
          -- and trims still used exports.
          depInfo <- lift (useNoFile_ GetModuleGraph)
          let revDeps = fromMaybe [] (transitiveReverseDependencies nfp depInfo)
          _ <- usesE GetModIfaceFromDiskAndIndex revDeps
          pure (pm, tmr)
      let avails = tcg_exports (tmrTypechecked tmr)
          excludeFp = [fromNormalizedFilePath nfp]
          hieDb = withHieDb (shakeExtras state)
      used <- liftIO $ filterM (isReferencedExternally hieDb excludeFp) avails
      pure $ setExportList (pm_parsed_source pm) (mapMaybe availToLIE used)
  case medits of
    Just edits ->
      pure $ ca & L.edit ?~ singleFileEdit uri edits
    Nothing ->
      throwError $ PluginInternalError "Export.Resolve: cannot locate module name span"

quickCodeActionHandlers :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
quickCodeActionHandlers state _plId (CodeActionParams _ _ doc range _) = do
  let uri = doc ^. L.uri
  nfp <- getNormalizedFilePathE uri
  pm <- runActionE "Export.GetParsedModuleWithComments" state (useE GetParsedModuleWithComments nfp)
  let ps = pm_parsed_source pm
  case (isExplicit ps, locateUnderCursor (range ^. L.start) ps) of
    (True, Just under) -> do
      -- The names GHC flags as defined-but-unused. Attach the action to the
      -- unused diagnostics as well.
      unusedDiags <- liftIO $ unusedTopBindDiagnostics state nfp
      pure . InL . map InR $
        [ ca
        | Just (verb, title, edits) <-
            [ addAction under ps
            , removeAction under ps
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
    isUnusedTopBind d =
      case d ^? fdStructuredMessageL . _SomeStructuredMessage . msgEnvelopeErrorL . _TcRnMessage of
        Just (TcRnUnusedName _ UnusedNameTopDecl) -> True
        _                                         -> False

addAction :: UnderCursor -> ParsedSource -> Maybe (Text, Text, [TextEdit])
addAction under ps = case under of
  Decl flavor n
    | n `isExported` ps -> Nothing
    | otherwise -> ("Export", printRdrName n,) <$> addExport ps (mkExportIE flavor n)
  Constructor t c
    | c `isExported` ps -> Nothing
    | otherwise ->
        ("Export", printRdrName t <> "(" <> printRdrName c <> ")",)
          <$> addConstructorExport t c ps
  Header -> Nothing

removeAction :: UnderCursor -> ParsedSource -> Maybe (Text, Text, [TextEdit])
removeAction under ps = case under of
  Decl _ n -> removeNamed n
  Constructor t c ->
    ("Unexport", printRdrName c,)
      <$> (removeConstructorExport t c ps <|> removeExport ps c)
  Header -> Nothing
  where
    removeNamed n
      | n `isExported` ps = ("Unexport", printRdrName n,) <$> removeExport ps n
      | otherwise = Nothing
