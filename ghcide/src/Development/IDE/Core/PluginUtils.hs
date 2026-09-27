{-# LANGUAGE GADTs #-}
module Development.IDE.Core.PluginUtils
(-- * Diagnostics
  activeDiagnosticsInRange
, activeDiagnosticsInRangeMT
, injectServerDiagnostics
-- * Formatting handlers
, mkFormattingHandlers) where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Error.Class         (MonadError (throwError))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Data.Text                         as T
import qualified Data.Text.Utf16.Rope.Mixed        as Rope
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.Service      (runAction)
import           Development.IDE.Core.Shake        (IdeState (shakeExtras))
import qualified Development.IDE.Core.Shake        as Shake
import           Development.IDE.GHC.Orphans       ()
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location    (NormalizedFilePath)
import           Ide.Plugin.Error
import           Ide.PluginUtils                   (asPosition, rangesOverlap)
import           Ide.Types
import qualified Language.LSP.Protocol.Lens        as LSP
import           Language.LSP.Protocol.Message     (SMethod (..))
import           Language.LSP.Protocol.Types       (CodeActionParams)
import qualified Language.LSP.Protocol.Types       as LSP
import qualified StmContainers.Map                 as STM

-- ----------------------------------------------------------------------------
-- Diagnostics
-- ----------------------------------------------------------------------------

-- | @'activeDiagnosticsInRangeMT' shakeExtras nfp range@ computes the
-- 'FileDiagnostic' 's that HLS produced and overlap with the given @range@.
--
-- This function is to be used whenever we need an authoritative source of truth
-- for which diagnostics are shown to the user.
-- These diagnostics can be used to provide various IDE features, for example
-- CodeActions, CodeLenses, or refactorings.
--
-- However, why do we need this when computing 'CodeAction's? A 'CodeActionParam'
-- has the 'CodeActionContext' which already contains the diagnostics!
-- But according to the LSP docs, the server shouldn't rely that these Diagnostic
-- are actually up-to-date and accurately reflect the state of the document.
--
-- From the LSP docs:
-- > An array of diagnostics known on the client side overlapping the range
-- > provided to the `textDocument/codeAction` request. They are provided so
-- > that the server knows which errors are currently presented to the user
-- > for the given range. There is no guarantee that these accurately reflect
-- > the error state of the resource. The primary parameter
-- > to compute code actions is the provided range.
--
-- Thus, even when the client sends us the context, we should compute the
-- diagnostics on the server side.
activeDiagnosticsInRangeMT :: MonadIO m => Shake.ShakeExtras -> NormalizedFilePath -> LSP.Range -> MaybeT m [FileDiagnostic]
activeDiagnosticsInRangeMT ide nfp range = do
    MaybeT $ liftIO $ atomically $ do
        mDiags <- STM.lookup (LSP.normalizedFilePathToUri nfp) (Shake.publishedDiagnostics ide)
        case mDiags of
            Nothing -> pure Nothing
            Just fileDiags -> do
                pure $ Just $ filter (diagRangeOverlaps range) fileDiags
    where
        -- The rationale of how to decide whether a 'Range' overlap with the
        -- 'Range' of a diagnostic is explained at
        -- https://github.com/haskell/haskell-language-server/pull/5047
        diagRangeOverlaps range fileDiag
          | Just c <- asPosition range
            -- The client sent us the cursor position, so we check if it
            -- overlaps with the **closed** 'Range' of the diagnostic.
            = LSP.positionInRange c diagRange || c == diagEnd
          | otherwise
            -- The client sent us a selection 'Range', so we check if it
            -- overlaps with the 'Range' of the diagnostic.
            = rangesOverlap range diagRange
          where
            diagRange@(LSP.Range _ diagEnd) = fileDiag ^. fdLspDiagnosticL . LSP.range

-- | Just like 'activeDiagnosticsInRangeMT'. See the docs of 'activeDiagnosticsInRangeMT' for details.
activeDiagnosticsInRange :: MonadIO m => Shake.ShakeExtras -> NormalizedFilePath -> LSP.Range -> m [FileDiagnostic]
activeDiagnosticsInRange ide nfp range = concat <$> runMaybeT (activeDiagnosticsInRangeMT ide nfp range)

-- Prefer server-side diagnostics if available; they are authoritative.
injectServerDiagnostics :: IdeState -> CodeActionParams -> IO CodeActionParams
injectServerDiagnostics ide params@LSP.CodeActionParams{_textDocument=LSP.TextDocumentIdentifier{_uri}, _range} = do
  serverDiags <- case LSP.uriToNormalizedFilePath (LSP.toNormalizedUri _uri) of
    Nothing  -> pure []
    Just nfp -> do
      mDiags <- activeDiagnosticsInRange (shakeExtras ide) nfp _range
      pure $ mDiags ^.. traverse . fdLspDiagnosticL
  pure $ params & LSP.context . LSP.diagnostics .~ serverDiags

-- ----------------------------------------------------------------------------
-- Formatting handlers
-- ----------------------------------------------------------------------------

-- `mkFormattingHandlers` was moved here from hls-plugin-api package so that
-- `mkFormattingHandlers` can refer to `IdeState`. `IdeState` is defined in the
-- ghcide package, but hls-plugin-api does not depend on ghcide, so `IdeState`
-- is not in scope there.

mkFormattingHandlers :: FormattingHandler IdeState -> PluginHandlers IdeState
mkFormattingHandlers f = mkPluginHandler SMethod_TextDocumentFormatting ( provider SMethod_TextDocumentFormatting)
                      <> mkPluginHandler SMethod_TextDocumentRangeFormatting (provider SMethod_TextDocumentRangeFormatting)
  where
    provider :: forall m. FormattingMethod m => SMethod m -> PluginMethodHandler IdeState m
    provider m ide _pid params
      | Just nfp <- LSP.uriToNormalizedFilePath $ LSP.toNormalizedUri uri = do
        contentsMaybe <- liftIO $ runAction "mkFormattingHandlers" ide $ getFileContents nfp
        case contentsMaybe of
          Just contents -> do
            let (typ, mtoken) = case m of
                  SMethod_TextDocumentFormatting -> (FormatText, params ^. LSP.workDoneToken)
                  SMethod_TextDocumentRangeFormatting -> (FormatRange (params ^. LSP.range), params ^. LSP.workDoneToken)
                  _ -> Prelude.error "mkFormattingHandlers: impossible"
            f ide mtoken typ (Rope.toText contents) nfp opts
          Nothing -> throwError $ PluginInvalidParams $ T.pack $ "Formatter plugin: could not get file contents for " ++ show uri

      | otherwise = throwError $ PluginInvalidParams $ T.pack $ "Formatter plugin: uriToFilePath failed for: " ++ show uri
      where
        uri = params ^. LSP.textDocument . LSP.uri
        opts = params ^. LSP.options
