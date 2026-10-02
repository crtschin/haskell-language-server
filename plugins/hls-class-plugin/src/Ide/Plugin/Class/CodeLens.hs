{-# LANGUAGE GADTs           #-}
{-# LANGUAGE OverloadedLists #-}
module Ide.Plugin.Class.CodeLens where

import           Control.Lens                  ((&), (<&>), (?~), (^.))
import           Control.Monad.Trans.Class     (MonadTrans (lift))
import           Data.Aeson                    hiding (Null)
import qualified Data.IntMap.Strict            as IntMap
import           Data.Maybe                    (maybeToList)
import qualified Data.Text                     as T
import           Development.IDE
import           Development.IDE.Core.API      (await, fastForward,
                                                fastForwardEach, lastKnown,
                                                runQuery, untrack)
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.Pragmas (getFirstPragma, insertNewPragma)
import           Ide.Plugin.Class.Types
import           Ide.Plugin.Class.Utils
import           Ide.Plugin.Error
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens    as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

-- The code lens method is only responsible for providing the ranges of the code
-- lenses matched to a unique id
codeLens :: PluginMethodHandler IdeState Method_TextDocumentCodeLens
codeLens state _plId clp = do
    nfp <-  getNormalizedFilePathE $ clp ^. L.textDocument . L.uri
    -- Using stale results means that we can almost always return a
    -- value. In practice this means the lenses don't 'flicker'
    lens <- runQuery state $ await $ lastKnown GetInstanceBindLens nfp
    let entries = fastForwardEach (lensRange . unResult <$> lens)
    pure $ InL $ map toCodeLens entries
    where toCodeLens (r, uniqueID) = CodeLens r Nothing (Just $ toJSON uniqueID)

-- The code lens resolve method matches a title to each unique id
codeLensResolve:: ResolveFunction IdeState Int Method_CodeLensResolve
codeLensResolve state plId cl uri uniqueID = do
    nfp <-  getNormalizedFilePathE uri
    (lens, tc, hsc) <- runQuery state $ await $ (,,)
      <$> lastKnown GetInstanceBindLens nfp
      <*> lastKnown TypeCheck nfp
      <*> (hscEnv <$> untrack <$> lastKnown GhcSession nfp)
    let pprCtx = untrack (mkPrintUnqualifiedDefault hsc . tcg_rdr_env . tmrTypechecked <$> tc)
    translated <- handleMaybe (PluginInvalidUserState "toCurrentRange") $ fastForward $
      lens <&> \l -> do
        entry@(_, name, typ) <- IntMap.lookup uniqueID $ lensDetails $ unResult l
        let title = toMethodName (printOutputable name) <> " :: " <> T.pack (showDoc hsc pprCtx typ)
        pure (title, makeEdit title entry)
    (title, edit) <- handleMaybe PluginStaleResolve translated
    let command = mkLspCommand plId typeLensCommandId title (Just [toJSON $ InstanceBindLensCommand uri edit])
    pure $ cl & L.command ?~ command
    where
        makeEdit :: T.Text -> (Range, Name, Type) -> TextEdit
        makeEdit bind (range, _, _) =
            let startPos = range ^. L.start
                insertChar = startPos ^. L.character
            in TextEdit (Range startPos startPos) (bind <> "\n" <> T.replicate (fromIntegral insertChar) " ")

unResult :: InstanceBindLensResult -> InstanceBindLens
unResult (InstanceBindLensResult l) = l

-- Finally the command actually generates and applies the workspace edit for the
-- specified unique id.
codeLensCommandHandler :: PluginId -> CommandFunction IdeState InstanceBindLensCommand
codeLensCommandHandler plId state _ InstanceBindLensCommand{commandUri, commandEdit} = do
    nfp <-  getNormalizedFilePathE commandUri
    lensEnabledExtensions <- runQuery state $ await $
      untrack . fmap (lensEnabledExtensions . unResult) <$> lastKnown GetInstanceBindLens nfp
    -- We are only interested in the pragma information if the user does not
    -- have the InstanceSigs extension enabled
    mbPragma <- if InstanceSigs `elem` lensEnabledExtensions
                then pure Nothing
                else Just <$> getFirstPragma plId state nfp
    let -- By mapping over our Maybe NextPragmaInfo value, we only compute this
        -- edit if we actually need to.
        pragmaInsertion =
            maybeToList $ flip insertNewPragma InstanceSigs <$> mbPragma
        wEdit = workspaceEdit pragmaInsertion
    _ <- lift $ pluginSendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wEdit) (\_ -> pure ())
    pure $ InR Null
    where
        workspaceEdit pragmaInsertion=
            WorkspaceEdit
                (pure [(commandUri, commandEdit : pragmaInsertion)])
                Nothing
                Nothing




