{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Export (Log (..), descriptor) where

import           Control.Applicative              ((<|>))
import           Control.Lens
import           Control.Monad                    (filterM)
import           Control.Monad.Error.Class        (throwError)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Aeson                       (toJSON)
import qualified Data.Map.Strict                  as Map
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Development.IDE
import           Development.IDE.Core.PluginUtils (runActionE, useE)
import           Development.IDE.Core.Shake       (ShakeExtras (..))
import qualified Development.IDE.Core.Shake       as Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Types.Shake      (WithHieDb)
import           HieDb                            (findReferences)
import           Ide.Plugin.Error                 (PluginError (..),
                                                   getNormalizedFilePathE)
import           Ide.Plugin.Export.Cursor
import           Ide.Plugin.Export.Exports
import           Ide.Plugin.Export.Utils
import           Ide.Plugin.Resolve
import           Ide.Types
import qualified Ide.Types                        as Ide
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message    (Method (..), SMethod (..))
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

explicitExportCodeActionProvider :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
explicitExportCodeActionProvider state _pId (CodeActionParams _ _ doc range _) = do
  nfp <- getNormalizedFilePathE (doc ^. L.uri)
  pm <-
    runActionE
      "Export.GetParsedModuleWithComments"
      state
      (useE GetParsedModuleWithComments nfp)
  let ps = pm_parsed_source pm
  pure . InL . map InR $
    case locateUnderCursor (range ^. L.start) ps of
      Just Header -> [mkAction "Export explicitly" & L.data_ ?~ toJSON ()]
      _           -> []

explicitExportCodeActionResolveProvider :: ResolveFunction IdeState () 'Method_CodeActionResolve
explicitExportCodeActionResolveProvider state _pId ca uri () = do
  nfp <- getNormalizedFilePathE uri
  (pm, tmr) <-
    runActionE "Export.Resolve" state $
      (,)
        <$> useE GetParsedModuleWithComments nfp
        <*> useE TypeCheck nfp
  let ps = pm_parsed_source pm
      avails = tcg_exports (tmrTypechecked tmr)
      excludeFp = [fromNormalizedFilePath nfp]
      hieDb = withHieDb (shakeExtras state)
  used <- liftIO $ filterM (availIsReferencedExternally hieDb excludeFp) avails
  let body = T.intercalate ", " (filter (not . T.null) (map renderAvail used))
  case setExportList ps body of
    Just edits ->
      pure $ ca & L.edit ?~ WorkspaceEdit (Just (Map.singleton uri edits)) Nothing Nothing
    Nothing ->
      throwError $ PluginInternalError "Export.Resolve: cannot locate module name span"

availIsReferencedExternally :: WithHieDb -> [FilePath] -> AvailInfo -> IO Bool
availIsReferencedExternally withDb exclude avail = do
  let n = availName avail
  case nameModule_maybe n of
    Nothing  -> pure False
    Just modl -> do
      rows <- withDb $ \db ->
        findReferences db True (nameOccName n) (Just (moduleName modl)) (Just (moduleUnit modl)) exclude
      pure (not (null rows))

renderAvail :: AvailInfo -> Text
renderAvail = \case
  AvailName n -> T.pack (printName n)
  AvailTC parent names _
    | null (filter (/= parent) names) -> T.pack (printName parent)
    | otherwise -> T.pack (printName parent) <> " (..)"
  AvailFL _ -> ""

quickCodeActionHandlers :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
quickCodeActionHandlers state _plId (CodeActionParams _ _ doc range _) = do
  let uri = doc ^. L.uri
  nfp <- getNormalizedFilePathE uri
  pm <- runActionE "Export.GetParsedModuleWithComments" state (useE GetParsedModuleWithComments nfp)
  let ps = pm_parsed_source pm
  pure . InL . map InR $ case (isExplicit ps, locateUnderCursor (range ^. L.start) ps) of
    (True, Just under) ->
      let mkWS edits = WorkspaceEdit (Just (Map.singleton uri edits)) Nothing Nothing
       in [ ca
          | Just (verb, title, edits) <-
              [ addAction under ps
              , removeAction under ps
              ]
          , let ca = mkAction (verb <> " `" <> title <> "`") & L.edit ?~ mkWS edits
          ]
    _ -> []

addAction :: UnderCursor -> ParsedSource -> Maybe (Text, Text, [TextEdit])
addAction under ps = case under of
  ValueOrSig n
    | n `isExported` ps -> Nothing
    | otherwise -> ("Export", printRdrName n,) <$> addExport ps (mkValueIE n)
  TypeDecl n
    | n `isExported` ps -> Nothing
    | otherwise -> ("Export", printRdrName n,) <$> addExport ps (mkTypeAllIE n)
  Constructor t c
    | c `isExported` ps -> Nothing
    | otherwise ->
        ("Export", printRdrName t <> "(" <> printRdrName c <> ")",)
          <$> addConstructorExport t c ps
  Header -> Nothing

removeAction :: UnderCursor -> ParsedSource -> Maybe (Text, Text, [TextEdit])
removeAction under ps = case under of
  ValueOrSig n -> removeNamed n
  TypeDecl n -> removeNamed n
  Constructor t c ->
    ("Unexport", printRdrName c,)
      <$> (removeConstructorExport t c ps <|> removeExport ps c)
  Header -> Nothing
  where
    removeNamed n
      | n `isExported` ps = ("Unexport", printRdrName n,) <$> removeExport ps n
      | otherwise = Nothing

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
