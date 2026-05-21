{-# LANGUAGE LambdaCase #-}
module Ide.Plugin.Export.Utils where

import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util

rdrNameFS :: RdrName -> FastString
rdrNameFS = occNameFS . rdrNameOcc

ieParentName :: IE GhcPs -> Maybe RdrName
ieParentName = \case
  IEVar _ (L _ wn) _           -> Just (ieWrappedRdrName wn)
  IEThingAbs _ (L _ wn) _      -> Just (ieWrappedRdrName wn)
  IEThingAll _ (L _ wn) _      -> Just (ieWrappedRdrName wn)
  IEThingWith _ (L _ wn) _ _ _ -> Just (ieWrappedRdrName wn)
  _                            -> Nothing

ieWrappedRdrName :: IEWrappedName GhcPs -> RdrName
ieWrappedRdrName = \case
  IEName _ (L _ rdr)    -> rdr
  IEPattern _ (L _ rdr) -> rdr
  IEType _ (L _ rdr)    -> rdr
