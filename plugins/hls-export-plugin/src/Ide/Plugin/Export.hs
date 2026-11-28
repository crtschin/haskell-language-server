module Ide.Plugin.Export (descriptor) where

import           Control.Concurrent.STM           (atomically)
import           Control.Lens
import           Control.Monad.IO.Class           (liftIO)
import           Data.Maybe                       (catMaybes, maybeToList)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Development.IDE
import           Development.IDE.Core.PluginUtils (runActionE)
import           Development.IDE.Core.Shake       (getDiagnostics)
import           Development.IDE.GHC.Compat.Error (_TcRnUnusedTopBind,
                                                   msgEnvelopeErrorL)
import           Ide.Plugin.Error                 (getNormalizedFilePathE)
import           Ide.Plugin.Export.Cursor
import           Ide.Plugin.Export.Exports
import           Ide.Plugin.Export.Utils
import           Ide.Types
import qualified Ide.Types                        as Ide
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message    (Method (..), SMethod (..))
import           Language.LSP.Protocol.Types

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId "Code actions for module export lists")
  { Ide.pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeAction codeActionProvider
  }

codeActionProvider :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeActionProvider state _plId (CodeActionParams _ _ doc range _) = do
  let uri = doc ^. L.uri
  nfp <- getNormalizedFilePathE uri
  msrc <- runActionE "Export.getSource" state (getSource nfp)
  actions <- case msrc of
    Just src | Just under <- locateUnderCursor (range ^. L.start) src.parsed ->
      liftIO (actionsAt state uri nfp src under)
    _ -> pure []
  pure (InL (map InR actions))

actionsAt :: IdeState -> Uri -> NormalizedFilePath -> Source -> UnderCursor -> IO [CodeAction]
actionsAt state uri nfp src = \case
  under@(Decl flavor n) -> do
    unused <- unusedTopBindDiagnostics state nfp
    let fixes = [ d | d <- unused, locateUnderCursor (d ^. L.range . L.start) src.parsed == Just under ]
    pure $ catMaybes
      [ withFixes fixes . action ("Export " <> quoted (printRdrName n)) <$> addExport src flavor n
      , action ("Unexport " <> quoted (printRdrName n)) <$> removeExport src n
      ]
  Constructor t c -> pure $ catMaybes
    [ action ("Export " <> quoted (printRdrName t <> "(" <> printRdrName c <> ")")) <$> addConstructorExport src t c
    , action ("Unexport " <> quoted (printRdrName c)) <$> removeConstructorExport src t c
    ]
  Header -> pure (maybeToList (action "Export all symbols" <$> exportAll src))
  where
    action title edits = mkAction title & L.edit ?~ singleFileEdit uri edits
    withFixes [] = id
    withFixes ds = L.diagnostics ?~ ds

unusedTopBindDiagnostics :: IdeState -> NormalizedFilePath -> IO [Diagnostic]
unusedTopBindDiagnostics state nfp = do
  diags <- atomically $ getDiagnostics state
  pure [ fdLspDiagnostic d | d <- diags, fdFilePath d == nfp, isUnusedTopBind d ]
  where
    isUnusedTopBind =
      has (fdStructuredMessageL . _SomeStructuredMessage . msgEnvelopeErrorL . _TcRnUnusedTopBind)

quoted :: String -> Text
quoted s = "`" <> T.pack s <> "`"
