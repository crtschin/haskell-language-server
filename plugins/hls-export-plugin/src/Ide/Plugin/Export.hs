{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Export (Log (..), descriptor) where

import           Control.Applicative                          ((<|>))
import           Control.Lens
import           Control.Monad                                (filterM)
import           Control.Monad.Error.Class                    (throwError)
import           Control.Monad.IO.Class                       (liftIO)
import           Control.Monad.Trans                          (lift)
import           Data.Aeson                                   (toJSON)
import qualified Data.Map.Strict                              as Map
import           Data.Maybe                                   (fromMaybe,
                                                               mapMaybe)
import           Data.Text                                    (Text)
import           Development.IDE
import           Development.IDE.Core.PluginUtils             (runActionE, useE,
                                                               usesE)
import           Development.IDE.Core.Shake                   (ShakeExtras (..))
import qualified Development.IDE.Core.Shake                   as Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Import.DependencyInformation (transitiveReverseDependencies)
import           Ide.Plugin.Error                             (PluginError (..),
                                                               getNormalizedFilePathE)
#ifdef hls_cabal
import           Ide.Plugin.Cabal.ExposedModules              (ExposedModuleCheck (..),
                                                               exposedModuleCheck)
#endif
import           Ide.Plugin.Export.Cursor
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Exports
import           Ide.Plugin.Resolve
import           Ide.Types
import qualified Ide.Types                                    as Ide
import qualified Language.LSP.Protocol.Lens                   as L
import           Language.LSP.Protocol.Message                (Method (..),
                                                               SMethod (..))
import           Language.LSP.Protocol.Types

data Log
  = LogShake Shake.Log
  | forall a. (Pretty a) => LogResolve a

instance Pretty Log where
  pretty (LogShake l)   = pretty l
  pretty (LogResolve l) = pretty l

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = do
  let resolveRecorder = cmapWithPrio LogResolve recorder
      (explicitExportCommand, explicitExportHandler) = mkCodeActionWithResolveAndCommand resolveRecorder plId explicitExportCodeActionProvider explicitExportCodeActionResolveProvider
      exportHandlers = mkPluginHandler SMethod_TextDocumentCodeAction quickCodeActionHandlers
  (defaultPluginDescriptor plId "Code actions for module export lists")
    { Ide.pluginHandlers = explicitExportHandler <> exportHandlers
    , Ide.pluginCommands = explicitExportCommand
    }

#ifndef hls_cabal
-- | Fallback when cabal support is not compiled in: every module is treated as
-- exposed, so the trim action is withheld rather than risking public API.
newtype ExposedModuleCheck = ExposedModuleCheck
  { isExposed :: NormalizedFilePath -> Bool }

exposedModuleCheck :: NormalizedFilePath -> IO ExposedModuleCheck
exposedModuleCheck _ = pure (ExposedModuleCheck (const True))
#endif

explicitExportCodeActionProvider :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
explicitExportCodeActionProvider state _pId (CodeActionParams _ _ doc range _) = do
  nfp <- getNormalizedFilePathE (doc ^. L.uri)
  pm <-
    runActionE
      "Export.GetParsedModuleWithComments"
      state
      (useE GetParsedModuleWithComments nfp)
  let ps = pm_parsed_source pm
  case locateUnderCursor (range ^. L.start) ps of
    Just Header -> do
      -- Withhold on public API: hiedb cannot see external consumers, so trimming exposed exports is unsafe.
      check <- liftIO $ exposedModuleCheck nfp
      pure . InL . map InR $
        [ mkAction "Export explicitly" & L.data_ ?~ toJSON ()
        | not (isExposed check nfp)
        ]
    _ -> pure (InL [])

explicitExportCodeActionResolveProvider :: ResolveFunction IdeState () 'Method_CodeActionResolve
explicitExportCodeActionResolveProvider state _pId ca uri () = do
  nfp <- getNormalizedFilePathE uri
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
  let ps = pm_parsed_source pm
      avails = tcg_exports (tmrTypechecked tmr)
      excludeFp = [fromNormalizedFilePath nfp]
      hieDb = withHieDb (shakeExtras state)
  used <- liftIO $ filterM (isReferencedExternally hieDb excludeFp) avails
  let items = mapMaybe availToLIE used
  case setExportList ps items of
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
  pure . InL . map InR $ case (isExplicit ps, locateUnderCursor (range ^. L.start) ps) of
    (True, Just under) ->
      [ ca
      | Just (verb, title, edits) <-
          [ addAction under ps
          , removeAction under ps
          ]
      , let ca = mkAction (verb <> " `" <> title <> "`") & L.edit ?~ singleFileEdit uri edits
      ]
    _ -> []

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

singleFileEdit :: Uri -> [TextEdit] -> WorkspaceEdit
singleFileEdit uri edits = WorkspaceEdit (Just (Map.singleton uri edits)) Nothing Nothing

mkAction :: Text -> CodeAction
mkAction title = CodeAction {..}
  where
    _title = title
    _kind = Just CodeActionKind_RefactorRewrite
    _diagnostics = Nothing
    _isPreferred = Nothing
    _disabled = Nothing
    _edit = Nothing
    _command = Nothing
    _data_ = Nothing
