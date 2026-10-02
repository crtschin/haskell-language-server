{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Class.Utils where

import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Except
import           Data.Char                       (isAlpha)
import qualified Data.Text                       as T
import           Development.IDE
import           Development.IDE.Core.API        (refresh_, runQuery, untrack)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util (fsLit)
import           Development.IDE.Spans.Pragmas   (getNextPragmaInfo,
                                                  insertNewPragma)
import           GHC.Utils.Outputable            (NamePprCtx)
import           Ide.Plugin.Error
import           Ide.PluginUtils
import           Language.LSP.Protocol.Types

-- | Check if some `HasSrcSpan` value is in the given range
inRange :: Range -> SrcSpan -> Bool
inRange range s = maybe False (subRange range) (srcSpanToRange s)

ghostSpan :: RealSrcSpan
ghostSpan = realSrcLocSpan $ mkRealSrcLoc (fsLit "<haskell-language-sever>") 1 1

showDoc :: HscEnv -> NamePprCtx -> Type -> String
showDoc hsc pprCtx ty = showSDocForUser' hsc pprCtx (pprSigmaType ty)

-- | Paren the name for pretty display if necessary
toMethodName :: T.Text -> T.Text
toMethodName n
    | Just (h, _) <- T.uncons n
    , not (isAlpha h || h == '_')
    = "(" <> n <> ")"
    | otherwise
    = n

-- | Here we use `useWithStale` to compute, Using stale results means that we can almost always return a value.
--   In practice this means the lenses don't 'flicker'.
--   This function is also used in code actions, but it doesn't matter because our actions only work
--   if the module parsed success.
insertPragmaIfNotPresent :: (MonadIO m)
    => IdeState
    -> NormalizedFilePath
    -> Extension
    -> ExceptT PluginError m [TextEdit]
insertPragmaIfNotPresent state nfp pragma = do
    sessionDynFlags <- untrack . fmap (hsc_dflags . hscEnv) <$> runQuery state (refresh_ GhcSession nfp)
    fileContents <- liftIO $ runAction "classplugin.insertPragmaIfNotPresent.GetFileContents" state
        $ getFileContents nfp
    exts <- untrack . fmap getExtensions <$> runQuery state (refresh_ GetParsedModuleWithComments nfp)
    let info = getNextPragmaInfo sessionDynFlags fileContents
    pure [insertNewPragma info pragma | pragma `notElem` exts]
