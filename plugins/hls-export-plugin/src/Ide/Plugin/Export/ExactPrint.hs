{-# LANGUAGE CPP #-}

module Ide.Plugin.Export.ExactPrint
  ( LExportList
  , mkExportIE
  , availToLIE
  , appendIE
  , removeMatchingIE
  , trimIEs
  , addCtorUnderParent
  , removeCtorUnderParent
  , printExportList
  , printIE
  , freshCtorEntry
  ) where

import           Control.Lens                              (_last, over)
import           Data.Bifunctor                            (first)
import           Data.List                                 (mapAccumL)
import           Data.List.NonEmpty                        (NonEmpty (..))
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Orphans               ()
#if MIN_VERSION_ghc(9,11,0)
import           GHC                                       (DeltaPos (..),
                                                            TrailingAnn (..))
#elif MIN_VERSION_ghc(9,9,0)
import           GHC                                       (DeltaPos (..),
                                                            LocatedL,
                                                            NoAnn (..),
                                                            TrailingAnn (..),
                                                            noAnn)
#else
import           GHC                                       (DeltaPos (..),
                                                            LocatedL,
                                                            TrailingAnn (..),
                                                            addAnns,
                                                            emptyComments,
                                                            noAnn)
#endif

import           Language.Haskell.GHC.ExactPrint           (addComma,
                                                            exactPrint,
                                                            getEntryDP,
                                                            setEntryDP)

#if MIN_VERSION_ghc(9,11,0)
import           GHC                                       (EpToken (..),
                                                            LocatedLI)
#else
import           GHC                                       (AddEpAnn (..))
#endif
import           Data.Maybe                                (fromMaybe,
                                                            isNothing,
                                                            listToMaybe)
import           Development.IDE.GHC.ExactPrint.Annotation (ensureTrailingComma,
                                                            epl, isCommaAnn,
                                                            parenthesizeName,
                                                            removeTrailingCommaAnn,
                                                            trailingAnns,
                                                            withTrailingComma)
import           GHC                                       (LocatedN)
import           Ide.Plugin.Export.Cursor                  (ExportFlavor (..))
import           Ide.Plugin.Export.Utils

-- | Located @[LIE GhcPs]@, the shape of an export list. Aliases either
-- 'LocatedL' (pre-9.12) or 'LocatedLI'.
#if MIN_VERSION_ghc(9,11,0)
type LExportList = LocatedLI [LIE GhcPs]
#else
type LExportList = LocatedL [LIE GhcPs]
#endif

-- | Render an 'AvailInfo' as an export item.
availToLIE :: (Name -> Bool) -> AvailInfo -> [LIE GhcPs]
availToLIE wanted = \case
  AvailName n -> [nameToIE n]
  AvailTC parent names pieces
    | not (null children), all wanted children -> [mkExportIE ExportAll parentRdr]  -- T(..)
    | c : cs <- filter wanted children -> [mkTypeWithIE parentRdr (nameRdr <$> c :| cs)]
    | otherwise                        -> [mkExportIE ExportFamily parentRdr]       -- T
    where
      parentRdr = nameRdr parent
      children = filter (/= parent) names ++ map flSelector pieces
  AvailFL fl -> [nameToIE (flSelector fl)]
  where
    nameToIE n
      | isDataOcc (nameOccName n) = mkExportIE ExportPattern (nameRdr n)
      | otherwise                 = mkExportIE ExportName (nameRdr n)
    nameRdr = mkRdrUnqual . nameOccName

data WrapKind = WrapPlain | WrapPattern | WrapType

mkExportIE :: ExportFlavor -> RdrName -> LIE GhcPs
mkExportIE flavor rdr = case flavor of
  ExportName    -> ieVar (mkWrappedName WrapPlain rdr)
  ExportPattern -> ieVar (mkWrappedName WrapPattern rdr)
  ExportFamily  -> mkTypeAbsIE (mkWrappedName keywordWrap rdr)
  ExportAll     -> mkTypeAllIE (mkWrappedName keywordWrap rdr)
  where
    keywordWrap
      | isSymOcc (rdrNameOcc rdr) = WrapType
      | otherwise                 = WrapPlain

ieVar :: LIEWrappedName GhcPs -> LIE GhcPs
ieVar w =
  reLocA $ L noSrcSpan $ IEVar
#if MIN_VERSION_ghc(9,8,0)
    Nothing
#else
    noExtField
#endif
    w
#if MIN_VERSION_ghc(9,9,0)
    Nothing
#endif

mkTypeAbsIE :: LIEWrappedName GhcPs -> LIE GhcPs
mkTypeAbsIE w =
  reLocA $ L noSrcSpan $ IEThingAbs
#if MIN_VERSION_ghc(9,11,0)
    Nothing
#elif MIN_VERSION_ghc(9,8,0)
    (Nothing, noAnn)
#else
    noAnn
#endif
    w
#if MIN_VERSION_ghc(9,9,0)
    Nothing
#endif

mkTypeAllIE :: LIEWrappedName GhcPs -> LIE GhcPs
mkTypeAllIE w =
  reLocA $ L noSrcSpan $ IEThingAll
#if MIN_VERSION_ghc(9,11,0)
    (Nothing, (EpTok (epl 1), EpTok (epl 0), EpTok (epl 0)))
#elif MIN_VERSION_ghc(9,9,0)
    ( Nothing
    , [ AddEpAnn AnnOpenP  (epl 1)
      , AddEpAnn AnnDotdot (epl 0)
      , AddEpAnn AnnCloseP (epl 0)
      ]
    )
#elif MIN_VERSION_ghc(9,8,0)
    ( Nothing
    , addAnns mempty
        [ AddEpAnn AnnOpenP  (epl 1)
        , AddEpAnn AnnDotdot (epl 0)
        , AddEpAnn AnnCloseP (epl 0)
        ]
        emptyComments
    )
#else
    (addAnns mempty
       [ AddEpAnn AnnOpenP  (epl 1)
       , AddEpAnn AnnDotdot (epl 0)
       , AddEpAnn AnnCloseP (epl 0)
       ]
       emptyComments)
#endif
    w
#if MIN_VERSION_ghc(9,9,0)
    Nothing
#endif

-- | @T(C1, C2, ...)@. The non-empty list is the child constructors.
mkTypeWithIE :: RdrName -> NonEmpty RdrName -> LIE GhcPs
mkTypeWithIE parent ctors =
  reLocA $ L noSrcSpan $ IEThingWith
#if MIN_VERSION_ghc(9,11,0)
    (Nothing, (EpTok (epl 1), NoEpTok, NoEpTok, EpTok (epl 0)))
#elif MIN_VERSION_ghc(9,9,0)
    (Nothing, [AddEpAnn AnnOpenP (epl 1), AddEpAnn AnnCloseP (epl 0)])
#elif MIN_VERSION_ghc(9,8,0)
    ( Nothing
    , addAnns mempty
        [AddEpAnn AnnOpenP (epl 1), AddEpAnn AnnCloseP (epl 0)]
        emptyComments
    )
#else
    (addAnns mempty
       [AddEpAnn AnnOpenP (epl 1), AddEpAnn AnnCloseP (epl 0)]
       emptyComments)
#endif
    (mkIEName parent)
    NoIEWildcard
    children
#if MIN_VERSION_ghc(9,9,0)
    Nothing
#endif
  where
    -- A separator comma is a trailing annotation, so every child except the
    -- last carries one.
    children = over _last (first removeTrailingCommaAnn)
                 (map (first addComma . mkIEName) (c : cs))
    c :| cs = ctors

mkIEName :: RdrName -> LIEWrappedName GhcPs
mkIEName = mkWrappedName WrapPlain

-- | Wrap an 'RdrName' as an export item. Operators are parenthesized and any
-- @pattern@ or @type@ keyword is followed by a single space.
mkWrappedName :: WrapKind -> RdrName -> LIEWrappedName GhcPs
mkWrappedName kind rdr =
  reLocA $ L noSrcSpan $ case kind of
    WrapPlain   -> IEName noExtField plainName
    WrapPattern -> IEPattern keywordTok spacedName
    WrapType    -> IEType keywordTok spacedName
  where
    plainName = parenthesizeOperator (reLocA (L noSrcSpan rdr))
    spacedName = setEntryDP plainName (SameLine 1)
    keywordTok =
#if MIN_VERSION_ghc(9,11,0)
      EpTok (epl 0)
#else
      epl 0
#endif

parenthesizeOperator :: LocatedN RdrName -> LocatedN RdrName
parenthesizeOperator ln
  | isSymOcc (rdrNameOcc (unLoc ln)) = parenthesizeName ln
  | otherwise = ln

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

-- | The trailing comma that separates existing items, if the list has any.
separatorComma :: [LIE GhcPs] -> Maybe TrailingAnn
separatorComma items =
  listToMaybe [c | L ann _ <- items, c <- trailingAnns ann, isCommaAnn c]

-- | Drop the first element matching @p@ and keep the list's layout. 'Nothing'
-- if nothing matches, @Just []@ if it was the sole element.
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

removeAllListItems
  :: (LocatedAn AnnListItem a -> Bool)
  -> [LocatedAn AnnListItem a]
  -> Maybe [LocatedAn AnnListItem a]
removeAllListItems p = fmap go . removeListItem p
  where
    go items = maybe items go (removeListItem p items)

removeMatchingIE :: (IE GhcPs -> Bool) -> LExportList -> Maybe LExportList
removeMatchingIE p (L l items) = L l <$> removeListItem (p . unLoc) items

-- | Drop the entries not in @retained@.
trimIEs :: Retained -> LExportList -> Maybe LExportList
trimIEs retained (L l items)
  | isNothing dropped, not trimmed = Nothing
  | otherwise                      = Just (L l kept)
  where
    dropped = removeAllListItems (not . retainsEntry retained . unLoc) items
    (trimmed, kept) = mapAccumL trimItem False (fromMaybe items dropped)
    trimItem changed item@(L loc ie) =
      maybe (changed, item) ((,) True . L loc) (trimChildren retained ie)

-- | Drop the children not in @retained@.
trimChildren :: Retained -> IE GhcPs -> Maybe (IE GhcPs)
trimChildren retained ie = do
  tw <- ieThingWithParts ie
  kept <- removeAllListItems (not . retained.child . lieWrappedNameFS) tw.children
  Just (rebuildThingWith tw kept)

rebuildThingWith :: ThingWith -> [LIEWrappedName GhcPs] -> IE GhcPs
rebuildThingWith tw []   = unLoc (mkTypeAbsIE (setEntryDP tw.parent (SameLine 0)))
rebuildThingWith tw kept = tw.rebuild kept

-- | 'Nothing' iff @ctor@ is already exported (via @T(..)@ or @T(...,ctor,...)@).
addCtorUnderParent ::
  -- | parent
  RdrName ->
  -- | ctor
  RdrName ->
  LExportList ->
  Maybe LExportList
addCtorUnderParent parent ctor lst@(L l items) =
  case ctorExportEdit parent ctor items of
    AlreadyExported -> Nothing
    AppendParent    -> Just (appendIE newThing lst)
    UpgradeBare     -> Just (L l (map (transformParent (const (unLoc newThing))) items))
    AddChild        -> Just (L l (map (transformParent (addCtorChildren ctor)) items))
  where
    newThing = mkTypeWithIE parent (ctor :| [])
    transformParent f (L itemLoc ie)
      | parentNameIs (rdrNameFS parent) ie = L itemLoc (f ie)
      | otherwise = L itemLoc ie

-- | Append @ctor@ to an @IEThingWith@'s children, reusing the sibling separator
-- comma.
addCtorChildren :: RdrName -> IE GhcPs -> IE GhcPs
addCtorChildren ctor ie = maybe ie grow (ieThingWithParts ie)
  where
    grow tw =
      let cs = tw.children
          hasSibling = not (null cs)
          newChild = setEntryDP (mkIEName ctor) (SameLine (if hasSibling then 1 else 0))
       in tw.rebuild ((if hasSibling then map (first ensureTrailingComma) cs else cs) ++ [newChild])

-- | Remove @ctor@ from the export entries listing it under @parent@, or
-- 'Nothing' if none does. Removing the last child downgrades @T(ctor)@ to @T@.
removeCtorUnderParent ::
  -- | parent
  RdrName ->
  -- | ctor
  RdrName ->
  LExportList ->
  Maybe LExportList
removeCtorUnderParent parent ctor (L l items)
  | edited    = Just (L l items')
  | otherwise = Nothing
  where
    (edited, items') = mapAccumL dropCtor False items
    parentFS = rdrNameFS parent
    ctorFS = rdrNameFS ctor
    isCtor = (== ctorFS) . lieWrappedNameFS

    dropCtor changed item@(L itemLoc ie)
      | parentNameIs parentFS ie
      , Just tw <- ieThingWithParts ie
      , Just kept <- removeListItem isCtor tw.children
      = (True, L itemLoc (rebuildThingWith tw kept))
      | otherwise = (changed, item)

printExportList :: LExportList -> Text
printExportList l = T.pack (exactPrint (setEntryDP l (SameLine 0)))

-- | Exactprint a single item, without the surrounding list layout.
printIE :: LIE GhcPs -> Text
printIE item = T.pack (exactPrint (setEntryDP (first removeTrailingCommaAnn item) (SameLine 0)))

-- | A fresh @T(ctor)@ export entry rendered as text, or 'Nothing' if @ctor@ is
-- already exported. See Note [Reprinting erases CPP directives].
freshCtorEntry :: RdrName -> RdrName -> [LIE GhcPs] -> Maybe Text
freshCtorEntry parent ctor items = case ctorExportEdit parent ctor items of
  AlreadyExported -> Nothing
  _               -> Just (printIE (mkTypeWithIE parent (ctor :| [])))

-- | How to add @ctor@ to an export list so its parent type @T@ exports it.
data CtorEdit
  = AlreadyExported  -- ^ @T(..)@ or @T(..., ctor, ...)@, nothing to do
  | AppendParent     -- ^ no entry for @T@ yet, add a fresh @T(ctor)@
  | UpgradeBare      -- ^ replace the bare @T@ entry with @T(ctor)@
  | AddChild         -- ^ add @ctor@ to the existing @T(...)@ entry

-- | Decide how to add @ctor@ under @parent@. We choose the first that matches.
ctorExportEdit :: RdrName -> RdrName -> [LIE GhcPs] -> CtorEdit
ctorExportEdit parent ctor items
  | any (parentNameIs ctorFS . unLoc) items = AlreadyExported
  | otherwise                               = go items
  where
    parentFS = rdrNameFS parent
    ctorFS = rdrNameFS ctor
    go [] = AppendParent
    go (L _ ie : rest)
      | parentNameIs parentFS ie = case ie of
          IEThingAll {} -> AlreadyExported
          IEThingAbs {} -> UpgradeBare
          _ | Just tw <- ieThingWithParts ie ->
                if any ((== ctorFS) . lieWrappedNameFS) tw.children
                  then AlreadyExported
                  else AddChild
            | otherwise -> go rest
      | otherwise = go rest
