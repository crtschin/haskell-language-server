{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}
module Ide.Plugin.Export.Utils where

import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util

rdrNameFS :: RdrName -> FastString
rdrNameFS = occNameFS . rdrNameOcc

ieParentName :: IE GhcPs -> Maybe RdrName
ieParentName e = case e of
#if MIN_VERSION_ghc(9,9,0)
  IEVar _ (L _ wn) _           -> Just (ieWrappedRdrName wn)
  IEThingAbs _ (L _ wn) _      -> Just (ieWrappedRdrName wn)
  IEThingAll _ (L _ wn) _      -> Just (ieWrappedRdrName wn)
  IEThingWith _ (L _ wn) _ _ _ -> Just (ieWrappedRdrName wn)
#else
  IEVar _ (L _ wn)             -> Just (ieWrappedRdrName wn)
  IEThingAbs _ (L _ wn)        -> Just (ieWrappedRdrName wn)
  IEThingAll _ (L _ wn)        -> Just (ieWrappedRdrName wn)
  IEThingWith _ (L _ wn) _ _   -> Just (ieWrappedRdrName wn)
#endif
  _                            -> Nothing

ieWrappedRdrName :: IEWrappedName GhcPs -> RdrName
ieWrappedRdrName = \case
  IEName _ (L _ rdr)    -> rdr
  IEPattern _ (L _ rdr) -> rdr
  IEType _ (L _ rdr)    -> rdr
#if MIN_VERSION_ghc(9,11,0)
  IEDefault _ (L _ rdr) -> rdr
#endif
