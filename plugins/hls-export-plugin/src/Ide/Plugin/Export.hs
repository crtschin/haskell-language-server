module Ide.Plugin.Export (descriptor) where

import           Control.Applicative                          ((<|>))
import           Control.Concurrent.STM
import           Control.Lens                                 hiding (uses)
import           Control.Monad
import           Control.Monad.Error.Class                    (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Aeson
import qualified Data.HashMap.Strict                          as HM
import           Data.List
import           Data.Maybe
import           Data.Text                                    (Text)
import qualified Data.Text                                    as T
import           Development.IDE
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake                   (HieDbWriter (..),
                                                               ShakeExtras (..))
import           Development.IDE.GHC.Compat
import           Development.IDE.Import.DependencyInformation
import           Ide.Plugin.Error
import           Ide.Plugin.Export.Cursor
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Exports
import           Ide.Plugin.Export.Utils
import           Ide.Plugin.Resolve
import           Ide.Types
import qualified Ide.Types                                    as Ide
import qualified Language.LSP.Protocol.Lens                   as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

data Log
  = forall a. Pretty a => LogResolve a

instance Pretty Log where
  pretty (LogResolve msg) = pretty msg

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  let resolveRecorder = cmapWithPrio LogResolve recorder
      -- Resolving is heavy, so a client without resolve support gets a command.
      (explicitExportCommands, explicitExportHandler) =
        mkCodeActionWithResolveAndCommand resolveRecorder plId
          explicitExportProvider explicitExportResolve
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
  mInputs <- runActionE "Export.getInputs" state $ do
    pm <- useE GetParsedModuleWithComments nfp
    let ps = pm_parsed_source pm
    case if isExplicit ps then locateUnderCursor (range ^. L.start) ps else Nothing of
      Nothing -> pure Nothing
      Just under
        -- Only a CPP module needs the text buffer, skip it otherwise.
        | isCpp (pm_mod_summary pm) ->
            fmap (\src -> (ps, under, Just src)) . snd <$> useE GetFileContents nfp
        | otherwise -> pure (Just (ps, under, Nothing))
  case mInputs of
    Just (ps, under, msrc)
      | Just el <- exportListOf msrc ps
      , actions@(_ : _) <- catMaybes [addAction el under, removeAction el under] -> do
          -- Attach the actions to the unused-binding diagnostics they would
          -- fix.
          unusedDiags <- liftIO $ unusedTopBindDiagnostics state nfp
          let fixes = [d | d <- unusedDiags, locateUnderCursor (d ^. L.range . L.start) ps == Just under]
          pure . InL . map InR $
            [ mkAction (verb <> " `" <> title <> "`")
                & L.edit ?~ singleFileEdit uri edits
                & L.diagnostics .~ (if null fixes then Nothing else Just fixes)
            | (verb, title, edits) <- actions
            ]
    _ -> pure (InL [])

addAction :: ExportList -> UnderCursor -> Maybe (Text, Text, [TextEdit])
addAction el under = case under of
  Decl flavor n -> ("Export", printRdrText n,) <$> addExport el (mkExportIE flavor n)
  Constructor t c ->
    ("Export", printRdrText t <> "(" <> printRdrText c <> ")",) <$> addConstructorExport el t c
  Header -> Nothing

removeAction :: ExportList -> UnderCursor -> Maybe (Text, Text, [TextEdit])
removeAction el under = case under of
  Decl _ n -> ("Unexport", printRdrText n,) <$> removeExport el n
  Constructor t c ->
    ("Unexport", printRdrText c,) <$> (removeConstructorExport el t c <|> standalone t c)
  Header -> Nothing
  where
    -- A bare uppercase entry denotes the type, so when the constructor shares
    -- the type's name, skip the standalone-removal fallback.
    standalone t c
      | rdrNameFS c == rdrNameFS t = Nothing
      | otherwise                  = removeExport el c

-- | Offer "Export explicitly" when the cursor is on the module header.
--
-- See Note [Every consumer must be visible].
explicitExportProvider :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
explicitExportProvider state _plId (CodeActionParams _ _ doc range _) = do
  nfp <- getNormalizedFilePathE (doc ^. L.uri)
  wholeProject <- isWholeProjectLoading state
  offer <- if wholeProject
    then runIdeActionE "Export.explicitly.offer" (shakeExtras state) $ do
      -- The checks only ask whether the cursor sits in the header and how the
      -- list is shaped, and a stale tree answers both.
      (pm, pmap) <- useWithStaleFastE GetParsedModuleWithComments nfp
      let ps = pm_parsed_source pm
          summ = pm_mod_summary pm
          -- Map the cursor back onto the stale tree.
          stalePosition = fromCurrentPosition pmap (range ^. L.start) >>= flip locateUnderCursor ps
      case stalePosition of
        Just Header -> do
          -- Only a CPP module's list can hold a directive, so skip the fetch
          -- otherwise.
          msrc <- if isCpp summ
            then snd . fst <$> useWithStaleFastE GetFileContents nfp
            else pure Nothing
          if unsafeToRefine summ msrc ps
            then pure False
            else not <$> lift (exposureCheckFast nfp (modNameText summ))
        _ -> pure False
    else pure False
  pure . InL $
    [InR (mkAction "Export explicitly" & L.data_ ?~ toJSON ExportUsed) | offer]

{- Note [Every consumer must be visible]

"Export explicitly" deletes exports conservatively. The action runs only where
every consumer is visible:
  - The config option @componentsLoading@ asks for whole-project loading.
  - No library exposes the module.
  - Every reverse dependency builds, so hiedb holds its references.

Two more checks to avoid edge-cases, these can be solved with enough tests:
  - The export list doesn't contain CPP directives.
    See Note [Reprinting erases CPP directives].
  - The export list doesn't re-export whole modules.
-}

-- | Resolve action for "Export explicitly".
--
-- See Note [Every consumer must be visible].
explicitExportResolve :: ResolveFunction IdeState ExportResolveData Method_CodeActionResolve
explicitExportResolve state _plId ca uri ExportUsed = do
  nfp <- getNormalizedFilePathE uri
  isWholeProject <- isWholeProjectLoading state
  unless isWholeProject $ throwError PluginStaleResolve

  (ps, avails, msrc, revDeps) <-
    runActionE "Export.explicitly.resolve" state $ do
      pm <- useE GetParsedModuleWithComments nfp
      tmr <- useE TypeCheck nfp
      depInfo <- lift (useNoFile_ GetModuleGraph)
      revDeps <-
        handleMaybe (PluginInternalError "This module is not in the loaded module graph")
          (transitiveReverseDependencies nfp depInfo)
      msrc <- snd <$> useE GetFileContents nfp
      let ps = pm_parsed_source pm
          summ = pm_mod_summary pm
      exposed <- lift (exposureCheck nfp (modNameText summ))
      -- The offer ran on stale results, so re-ask on current ones.
      -- See Note [Every consumer must be visible].
      when (exposed || unsafeToRefine summ msrc ps) $ throwError PluginStaleResolve
      -- Rebuild and re-index every reverse dependency.
      built <- lift (uses GetModIfaceFromDiskAndIndex revDeps)
      case [dep | (dep, Nothing) <- zip revDeps built] of
        [] -> pure ()
        broken@(worst : _) -> throwError . PluginInvalidUserState $
          T.pack (show (length broken))
            <> " module(s) importing this one do not build, starting with "
            <> T.pack (fromNormalizedFilePath worst)
      pure (ps, tcg_exports (tmrTypechecked tmr), msrc, revDeps)

  liftIO $ atomically $ do
    pending <- readTVar (indexPending (hiedbWriter (shakeExtras state)))
    check (not (any (`HM.member` pending) revDeps))

  used <- liftIO $ filterM
    (isReferencedExternally (withHieDb (shakeExtras state)) [fromNormalizedFilePath nfp])
    avails
  let refreshed
        -- An export is kept when any name it brings into scope is used.
        | Just el <- exportListOf msrc ps =
            retainExports el (map (occNameFS . nameOccName) (concatMap availNames used))
        | isExplicit ps = Nothing
        -- An implicit list can be cleanly regenerated.
        | otherwise = addExportList ps (sortOn lexicalOrder (concatMap availToLIE used))
  edits <- handleMaybe (PluginInternalError "Cannot rewrite the export list") refreshed

  pure $ ca & L.edit ?~ singleFileEdit uri edits
