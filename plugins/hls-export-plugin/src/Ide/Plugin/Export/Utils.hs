{-# LANGUAGE CPP #-}
module Ide.Plugin.Export.Utils where

import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import qualified GHC.LanguageExtensions.Type     as LangExt
import           Language.LSP.Protocol.Types

rdrNameFS :: RdrName -> FastString
rdrNameFS = occNameFS . rdrNameOcc

-- | The head and the listed children of a @T(C1, C2)@ entry, with a way to
-- replace the children.
ieThingWith
  :: IE GhcPs
  -> Maybe (LIEWrappedName GhcPs, [LIEWrappedName GhcPs], [LIEWrappedName GhcPs] -> IE GhcPs)
#if MIN_VERSION_ghc(9,9,0)
ieThingWith (IEThingWith x n w cs docs) = Just (n, cs, \cs' -> IEThingWith x n w cs' docs)
#else
ieThingWith (IEThingWith x n w cs)      = Just (n, cs, \cs' -> IEThingWith x n w cs')
#endif
ieThingWith _                           = Nothing

parentNameIs :: FastString -> IE GhcPs -> Bool
parentNameIs fs ie = case ieNames ie of
  n : _ -> rdrNameFS n == fs
  []    -> False

lieWrappedNameFS :: LIEWrappedName GhcPs -> FastString
lieWrappedNameFS = rdrNameFS . ieWrappedName . unLoc

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

isCppModule :: ModSummary -> Bool
isCppModule = xopt LangExt.Cpp . ms_hspp_opts
