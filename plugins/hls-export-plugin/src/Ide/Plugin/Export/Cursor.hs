{-# LANGUAGE LambdaCase #-}

module Ide.Plugin.Export.Cursor (UnderCursor (..), locateUnderCursor) where

import           Control.Applicative        ((<|>))
import           Data.Foldable              (toList)
import           Data.List                  (find)
import           Data.Maybe
import           Development.IDE
import           Development.IDE.GHC.Compat

data UnderCursor
  = ValueOrSig RdrName
  | TypeDecl RdrName
  | Constructor RdrName RdrName
  | Header

locateUnderCursor :: Position -> ParsedSource -> Maybe UnderCursor
locateUnderCursor pos ps = classifyHeader pos (unLoc ps) <|> classifyInDecl
  where
    classifyInDecl = do
      L _ decl <- find (\(L l _) -> pos `isInsideSrcSpan` locA l) (hsmodDecls (unLoc ps))
      classifyDecl pos decl

-- | Match column-free so cursor anywhere on the @module ... where@ line counts.
classifyHeader :: Position -> HsModule GhcPs -> Maybe UnderCursor
classifyHeader pos mod = inName <|> inExports
  where
    isIn :: HasSrcSpan a => Maybe a -> Maybe UnderCursor
    isIn el = el >>= \n -> if pos `isInsideSrcSpanLines` getLoc n then Just Header else Nothing
    inName = isIn $ hsmodName mod
    inExports = isIn $ hsmodExports mod

classifyDecl :: Position -> HsDecl GhcPs -> Maybe UnderCursor
classifyDecl pos = \case
  ValD _ FunBind {fun_id = lname}
    | onName lname -> Just (ValueOrSig (unLoc lname))
  SigD _ (TypeSig _ names _) ->
    ValueOrSig . unLoc <$> listToMaybe (filter onName names)
  TyClD _ DataDecl {tcdLName = lname, tcdDataDefn = HsDataDefn {dd_cons = cons}}
    | Just c <- constructorUnderCursor pos cons -> Just (Constructor (unLoc lname) c)
    | onName lname -> Just (TypeDecl (unLoc lname))
  TyClD _ SynDecl {tcdLName = lname}
    | onName lname -> Just (TypeDecl (unLoc lname))
  TyClD _ ClassDecl {tcdLName = lname}
    | onName lname -> Just (TypeDecl (unLoc lname))
  _ -> Nothing
  where
    onName (L l _) = pos `isInsideSrcSpan` locA l

constructorUnderCursor :: Position -> DataDefnCons (LConDecl GhcPs) -> Maybe RdrName
constructorUnderCursor pos cons =
  listToMaybe . mapMaybe nameAt $ extract_cons cons
  where
    nameAt (L _ cd) =
      listToMaybe [n | L l n <- conDeclNames cd, pos `isInsideSrcSpan` locA l]

    conDeclNames = \case
      ConDeclH98 {con_name = lname} -> [lname]
      ConDeclGADT {con_names = lnames} -> toList lnames
      _ -> []
