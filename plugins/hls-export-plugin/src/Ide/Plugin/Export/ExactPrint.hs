{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.Export.ExactPrint
  ( LExportList
  , renderExport
  , parseIE
  , appendIE
  , removeMatchingIE
  , addCtorUnderParent
  , removeCtorUnderParent
  , printExportList
  , freshCtorEntry
  ) where

import           Control.Lens                              (_last, over)
import           Control.Monad                             (guard)
import           Data.Bifunctor                            (first)
import           Data.Maybe                                (listToMaybe)
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Data.Traversable                          (mapAccumM)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint.Annotation (ensureTrailingComma,
                                                            isCommaAnn,
                                                            removeTrailingCommaAnn,
                                                            trailingAnns,
                                                            withTrailingComma)
import           Development.IDE.GHC.Orphans               ()
import           Development.IDE.GHC.Util                  (printRdrName)
#if MIN_VERSION_ghc(9,11,0)
import           GHC                                       (DeltaPos (..),
                                                            LocatedLI,
                                                            TrailingAnn (..))
#else
import           GHC                                       (DeltaPos (..),
                                                            LocatedL,
                                                            TrailingAnn (..))
#endif
import qualified GHC.LanguageExtensions.Type               as LangExt
import           Ide.Plugin.Export.Cursor                  (ExportFlavor (..))
import           Ide.Plugin.Export.Utils
import           Language.Haskell.GHC.ExactPrint           (exactPrint,
                                                            getEntryDP,
                                                            makeDeltaAst,
                                                            setEntryDP)
import           Language.Haskell.GHC.ExactPrint.Parsers   (parseWith)

-- | Located @[LIE GhcPs]@, the shape of an export list. Aliases either
-- 'LocatedL' (pre-9.12) or 'LocatedLI'.
#if MIN_VERSION_ghc(9,11,0)
type LExportList = LocatedLI [LIE GhcPs]
#else
type LExportList = LocatedL [LIE GhcPs]
#endif

renderExport :: ExportFlavor -> RdrName -> Text
renderExport flavor rdr = case flavor of
  ExportName    -> name
  ExportPattern -> "pattern " <> name
  ExportAbs     -> keyword <> name
  ExportAll     -> keyword <> name <> " (..)"
  where
    name = T.pack (printRdrName rdr)
    keyword
      | isSymOcc (rdrNameOcc rdr) = "type "
      | otherwise                 = ""

renderTypeWith :: RdrName -> [RdrName] -> Text
renderTypeWith parent ctors =
  T.pack (printRdrName parent) <> " (" <> T.intercalate ", " (T.pack . printRdrName <$> ctors) <> ")"

-- | Parse an export item through a throwaway module header.
parseIE :: DynFlags -> Text -> Maybe (LIE GhcPs)
parseIE dflags item =
  case parseWith (foldl xopt_set dflags [LangExt.PatternSynonyms, LangExt.ExplicitNamespaces])
         "" parseModule (T.unpack ("module M (" <> item <> ") where")) of
    Right ps | Just (L _ [ie]) <- hsmodExports (unLoc (makeDeltaAst ps)) -> Just ie
    _ -> Nothing

appendIE :: LIE GhcPs -> LExportList -> LExportList
appendIE item (L l items) = L l (fixLast items ++ [newItem (not (null items))])
  where
    newItem hasSibling =
      setEntryDP (first removeTrailingCommaAnn item) (SameLine (if hasSibling then 1 else 0))
    -- Reuse the comma that already separates the list's items. On a multiline
    -- leading comma list that comma carries a 'DifferentLine' delta, so the new
    -- separator lands on its own line instead of collapsing onto the last item.
    fixLast = over _last (first addSep)
    addSep = maybe ensureTrailingComma withTrailingComma (separatorComma items)

separatorComma :: [LIE GhcPs] -> Maybe TrailingAnn
separatorComma items =
  listToMaybe [c | L ann _ <- items, c <- trailingAnns ann, isCommaAnn c]

-- | Drop the first element matching @p@. A removed head hands its entry delta
-- to the next element so the list keeps its start, and the new last element
-- loses its separator comma.
-- - @Nothing@ if nothing matches
-- - @Just []@ if the sole element was removed.
removeListItem
  :: (LocatedAn AnnListItem a -> Bool)
  -> [LocatedAn AnnListItem a]
  -> Maybe [LocatedAn AnnListItem a]
removeListItem p items = case break p items of
  (_, [])               -> Nothing
  (pre, removed : post) ->
    let survivors = case (pre, post) of
          ([], next : rest) -> setEntryDP next (getEntryDP removed) : rest
          _                 -> pre ++ post
     in Just (over _last (first removeTrailingCommaAnn) survivors)

removeMatchingIE :: (IE GhcPs -> Bool) -> LExportList -> Maybe LExportList
removeMatchingIE p (L l items) = L l <$> removeListItem (p . unLoc) items

-- | 'Nothing' iff @ctor@ is already exported (via @T(..)@ or @T(...,ctor,...)@).
addCtorUnderParent ::
  DynFlags ->
  -- | parent
  RdrName ->
  -- | ctor
  RdrName ->
  LExportList ->
  Maybe LExportList
addCtorUnderParent dflags parent ctor lst@(L l items) = do
  newThing <- parseIE dflags (renderTypeWith parent [ctor])
  (_, [newChild], _) <- ieThingWith (unLoc newThing)
  case ctorExportEdit parent ctor items of
    AlreadyExported -> Nothing
    AppendParent    -> Just (appendIE newThing lst)
    UpgradeBare     -> Just (L l (map (transformParent (const (unLoc newThing))) items))
    AddChild        -> Just (L l (map (transformParent (addChild newChild)) items))
  where
    transformParent f (L itemLoc ie)
      | parentNameIs (rdrNameFS parent) ie = L itemLoc (f ie)
      | otherwise = L itemLoc ie

addChild :: LIEWrappedName GhcPs -> IE GhcPs -> IE GhcPs
addChild child ie = case ieThingWith ie of
  Just (_, cs, setChildren) ->
    let hasSibling = not (null cs)
        newChild = setEntryDP child (SameLine (if hasSibling then 1 else 0))
     in setChildren ((if hasSibling then map (first ensureTrailingComma) cs else cs) ++ [newChild])
  Nothing -> ie

-- | Remove @ctor@ from the export entries listing it under @parent@, or
-- 'Nothing' if none does. Removing the last child downgrades @T(ctor)@ to @T@.
removeCtorUnderParent ::
  DynFlags ->
  -- | parent
  RdrName ->
  -- | ctor
  RdrName ->
  LExportList ->
  Maybe LExportList
removeCtorUnderParent dflags parent ctor (L l items) = do
  (edited, items') <- mapAccumM dropCtor False items
  guard edited
  Just (L l items')
  where
    parentFS = rdrNameFS parent
    isCtor = (== rdrNameFS ctor) . lieWrappedNameFS

    dropCtor changed item@(L itemLoc ie)
      | parentNameIs parentFS ie
      , Just (thing, children, setChildren) <- ieThingWith ie
      , Just kept <- removeListItem isCtor children
      = (,) True . L itemLoc <$>
          if null kept
            then unLoc <$> parseIE dflags (renderExport ExportAbs (ieWrappedName (unLoc thing)))
            else Just (setChildren kept)
      | otherwise = Just (changed, item)

printExportList :: LExportList -> Text
printExportList l = T.pack (exactPrint (setEntryDP l (SameLine 0)))

-- | A fresh @T(ctor)@ export entry rendered as text, or 'Nothing' if @ctor@ is
-- already exported in the parsed list.
--
-- Under CPP this adds a standalone entry so the splice doesn't reprint an
-- existing @T(...)@ span, which could collide with a directive.
freshCtorEntry :: RdrName -> RdrName -> [LIE GhcPs] -> Maybe Text
freshCtorEntry parent ctor items = case ctorExportEdit parent ctor items of
  AlreadyExported -> Nothing
  _               -> Just (renderTypeWith parent [ctor])

data CtorEdit
  = AlreadyExported
  | AppendParent
  | UpgradeBare
  | AddChild

ctorExportEdit :: RdrName -> RdrName -> [LIE GhcPs] -> CtorEdit
ctorExportEdit parent ctor = go
  where
    parentFS = rdrNameFS parent
    ctorFS = rdrNameFS ctor
    go [] = AppendParent
    go (L _ ie : rest)
      | parentNameIs parentFS ie = case ie of
          IEThingAll {} -> AlreadyExported
          IEThingAbs {} -> UpgradeBare
          _ | Just (_, cs, _) <- ieThingWith ie ->
                if any ((== ctorFS) . lieWrappedNameFS) cs then AlreadyExported else AddChild
            | otherwise -> go rest
      | otherwise = go rest
