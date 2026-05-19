{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Export (Log, descriptor) where

import           Control.Applicative              ((<|>))
import           Control.Lens
import qualified Data.Map.Strict                  as Map
import           Data.Text                        (Text)
import           Development.IDE
import           Development.IDE.Core.PluginUtils (runActionE, useE)
import qualified Development.IDE.Core.Shake       as Shake
import           Development.IDE.GHC.Compat
import           Ide.Plugin.Error                 (getNormalizedFilePathE)
import           Ide.Plugin.Export.Cursor
import           Ide.Plugin.Export.Exports
import           Ide.Types
import qualified Ide.Types                        as Ide
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message    (Method (..), SMethod (..))
import           Language.LSP.Protocol.Types

data Log = LogShake Shake.Log
  deriving (Show)

instance Pretty Log where
  pretty (LogShake l) = pretty l

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor _recorder plId =
  (defaultPluginDescriptor plId "Code actions for module export lists")
    { Ide.pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeAction codeActionHandler
    }

codeActionHandler :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeActionHandler state _plId (CodeActionParams _ _ doc range _) = do
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
    | otherwise -> ("Export", rdrNameText n,) <$> addExport ps (rdrNameText n)
  TypeDecl n
    | n `isExported` ps -> Nothing
    | otherwise -> ("Export", rdrNameText n,) <$> addExport ps (rdrNameText n <> " (..)")
  Constructor t c
    | c `isExported` ps -> Nothing
    | otherwise ->
        ("Export", rdrNameText t <> "(" <> rdrNameText c <> ")",)
          <$> addConstructorExport t c ps

removeAction :: UnderCursor -> ParsedSource -> Maybe (Text, Text, [TextEdit])
removeAction under ps = case under of
  ValueOrSig n -> removeNamed n
  TypeDecl n -> removeNamed n
  Constructor t c ->
    ("Unexport", rdrNameText c,)
      <$> (removeConstructorExport t c ps <|> removeExport ps c)
  where
    removeNamed n
      | n `isExported` ps = ("Unexport", rdrNameText n,) <$> removeExport ps n
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
