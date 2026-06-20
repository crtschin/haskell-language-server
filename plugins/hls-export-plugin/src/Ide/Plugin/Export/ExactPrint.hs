{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.Export.ExactPrint
  ( LExportList
  , mkExportIE
  , mkExportList
  , appendIE
  , removeMatchingIE
  , expandExportList
  , addCtorUnderParent
  , removeCtorUnderParent
  , printExportList
  , printIE
  , freshCtorEntry
  ) where

import           Control.Lens                              (_last, over)
import           Data.Bifunctor                            (first)
#if MIN_VERSION_ghc(9,10,0)
import           Data.List                                 (mapAccumL)
#else
import           Data.List                                 (foldl', mapAccumL)
#endif
import           Data.List.NonEmpty                        (NonEmpty (..))
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Orphans               ()
#if MIN_VERSION_ghc(9,11,0)
import           GHC                                       (AnnList (..),
                                                            DeltaPos (..),
                                                            EpAnn (..),
                                                            TrailingAnn (..),
                                                            emptyComments)
#elif MIN_VERSION_ghc(9,9,0)
import           GHC                                       (AnnList (..),
                                                            DeltaPos (..),
                                                            EpAnn (..),
                                                            LocatedL,
                                                            NoAnn (..),
                                                            TrailingAnn (..),
                                                            emptyComments,
                                                            noAnn)
#else
import           GHC                                       (AnnList (..),
                                                            DeltaPos (..),
                                                            EpAnn (..),
                                                            LocatedL,
                                                            SrcSpanAnn' (..),
                                                            TrailingAnn (..),
                                                            addAnns,
                                                            emptyComments,
                                                            noAnn)
#endif

#if !MIN_VERSION_ghc(9,9,0)
import           GHC.Parser.Annotation                     (Anchor (..),
                                                            AnchorOperation (..),
                                                            placeholderRealSpan)
#endif
import           Language.Haskell.GHC.ExactPrint           (addComma,
                                                            exactPrint,
                                                            setEntryDP,
                                                            transferEntryDP)
#if !MIN_VERSION_ghc(9,9,0)
import           Language.Haskell.GHC.ExactPrint           (runTransform)
#endif

#if MIN_VERSION_ghc(9,11,0)
import           GHC                                       (AnnListBrackets (..),
                                                            EpToken (..),
                                                            LocatedLI)
#else
import           GHC                                       (AddEpAnn (..))
#endif
import           Data.Maybe                                (listToMaybe)
import           Development.IDE.GHC.ExactPrint.Annotation (ensureTrailingComma,
                                                            epl, isCommaAnn,
                                                            parenthesizeOperatorName,
                                                            removeTrailingCommaAnn,
                                                            trailingAnns,
                                                            withTrailingComma)
import           Ide.Plugin.Export.Cursor                  (ExportFlavor (..))
import           Ide.Plugin.Export.Utils

-- | Located @[LIE GhcPs]@, the shape of an export list. Aliases either
-- 'LocatedL' (pre-9.12) or 'LocatedLI'.
#if MIN_VERSION_ghc(9,11,0)
type LExportList = LocatedLI [LIE GhcPs]
#else
type LExportList = LocatedL [LIE GhcPs]
#endif

mkExportIE :: ExportFlavor -> RdrName -> LIE GhcPs
mkExportIE flavor rdr = case flavor of
  ExportName    -> ieVar (mkWrappedName WrapPlain rdr)
  ExportPattern -> ieVar (mkWrappedName WrapPattern rdr)
  ExportFamily  -> mkTypeAbsIE' (mkWrappedName keywordWrap rdr)
  ExportAll     -> mkTypeAllIE' (mkWrappedName keywordWrap rdr)
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

-- | Bare @T@ (no constructors listed), reusing a wrapped head name.
mkTypeAbsIE' :: LIEWrappedName GhcPs -> LIE GhcPs
mkTypeAbsIE' w =
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

mkTypeAllIE' :: LIEWrappedName GhcPs -> LIE GhcPs
mkTypeAllIE' w =
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
    children = mkIEName c : map (first addComma . mkIEName) cs
    c :| cs = ctors

-- | Build a fresh located @(item1, item2, ...)@ from a list of items.
mkExportList :: [LIE GhcPs] -> LExportList
mkExportList items =
#if MIN_VERSION_ghc(9,9,0)
  L (EpAnn (epl 1) listAnn emptyComments) (commaList items)
#else
  L (SrcSpanAnn (EpAnn (Anchor placeholderRealSpan (MovedAnchor (SameLine 1))) listAnn emptyComments) noSrcSpan) (commaList items)
#endif
  where
#if MIN_VERSION_ghc(9,11,0)
    listAnn :: AnnList (EpToken "hiding", [EpToken ","])
    listAnn = AnnList
      { al_anchor = Nothing
      , al_brackets = ListParens (EpTok (epl 0)) (EpTok (epl 0))
      , al_semis = []
      , al_rest = (NoEpTok, [])
      , al_trailing = []
      }
#else
    listAnn :: AnnList
    listAnn = AnnList
      { al_anchor = Nothing
      , al_open = Just (AddEpAnn AnnOpenP (epl 0))
      , al_close = Just (AddEpAnn AnnCloseP (epl 0))
      , al_rest = []
      , al_trailing = []
      }
#endif

    commaList = go True
      where
        go _       []     = []
        go isFirst [x]    = [setEntryDP (first removeTrailingCommaAnn x) (dp isFirst)]
        go isFirst (x:xs) = setEntryDP (first ensureTrailingComma x) (dp isFirst) : go False xs
        dp isFirst = SameLine (if isFirst then 0 else 1)

-- | Map over an @IEThingWith@'s listed constructors, a no-op for any other item.
overThingWithChildren :: ([LIEWrappedName GhcPs] -> [LIEWrappedName GhcPs]) -> IE GhcPs -> IE GhcPs
#if MIN_VERSION_ghc(9,9,0)
overThingWithChildren f (IEThingWith x n w cs docs) = IEThingWith x n w (f cs) docs
#else
overThingWithChildren f (IEThingWith x n w cs)      = IEThingWith x n w (f cs)
#endif
overThingWithChildren _ ie                          = ie

data WrapKind = WrapPlain | WrapPattern | WrapType

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
    plainName = parenthesizeOperatorName (reLocA (L noSrcSpan rdr))
    spacedName = setEntryDP plainName (SameLine 1)
    keywordTok =
#if MIN_VERSION_ghc(9,11,0)
      EpTok (epl 0)
#else
      epl 0
#endif

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

-- | Pure 'transferEntryDP'. ghc-exactprint exposes it purely from 9.9 on;
-- earlier it lives in 'TransformT', so run the transform to recover the result.
transferItemDP :: LIE GhcPs -> LIE GhcPs -> LIE GhcPs
#if MIN_VERSION_ghc(9,9,0)
transferItemDP = transferEntryDP
#else
transferItemDP a b = let (r, _, _) = runTransform (transferEntryDP a b) in r
#endif

removeMatchingIE :: (IE GhcPs -> Bool) -> LExportList -> Maybe LExportList
removeMatchingIE p (L l items) = case break matches items of
  (_, []) -> Nothing
  (pre, removed : post) ->
    let kept = pre ++ post
        kept' = over _last (first removeTrailingCommaAnn) kept
        kept'' = if null pre then reuseHeadDP removed kept' else kept'
    in Just (L l kept'')
  where
    matches (L _ ie) = p ie
    -- The new first item inherits the removed head's entry delta and any
    -- preceding comments.
    reuseHeadDP _       []     = []
    reuseHeadDP removed (x:xs) = transferItemDP removed x : xs

-- | Append every desired item whose parent is not already listed, leaving the
-- existing items and their layout untouched.
expandExportList :: [LIE GhcPs] -> LExportList -> LExportList
expandExportList desired lst@(L _ items0) = foldl' (flip appendIE) lst additions
  where
    existingFSs = [rdrNameFS p | L _ ie <- items0, Just p <- [ieParentName ie]]
    additions   = [item | item@(L _ ie) <- desired, Just p <- [ieParentName ie], rdrNameFS p `notElem` existingFSs]

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
-- comma. No-op for other items.
addCtorChildren :: RdrName -> IE GhcPs -> IE GhcPs
addCtorChildren ctor = overThingWithChildren $ \cs ->
  let hasSibling = not (null cs)
      newChild = setEntryDP (mkIEName ctor) (SameLine (if hasSibling then 1 else 0))
   in (if hasSibling then map (first ensureTrailingComma) cs else cs) ++ [newChild]

-- | Removing the last child downgrades the parent from @T(ctor)@ to @T@.
removeCtorUnderParent ::
  RdrName {- ^ parent -} ->
  RdrName {- ^ ctor -} ->
  LExportList ->
  Maybe LExportList
removeCtorUnderParent parent ctor (L l items) =
  if changed then Just (L l newItems) else Nothing
  where
    (changed, newItems) = mapAccumL step False items
    parentFS = rdrNameFS parent
    ctorFS = rdrNameFS ctor

    step acc lie@(L itemLoc ie)
      | parentNameIs parentFS ie
      , Just cs <- ieThingWithChildren ie
      , any ((== ctorFS) . lieWrappedNameFS) cs =
          let cs' = filter ((/= ctorFS) . lieWrappedNameFS) cs
              ie' = case cs' of
                      [] -> downgradeToAbs ie
                      _  -> overThingWithChildren (const (normaliseChildren cs')) ie
          in (True, L itemLoc ie')
      | otherwise = (acc, lie)

    -- Reuse the original head so a @type@ keyword or operator wrapping survives
    -- the downgrade (e.g. @type (:<)(C)@ becomes @type (:<)@, not @(:<)@).
    downgradeToAbs :: IE GhcPs -> IE GhcPs
    downgradeToAbs ie = case ieThingWithHead ie of
      Just n  -> unLoc (mkTypeAbsIE' (setEntryDP n (SameLine 0)))
      Nothing -> ie

    normaliseChildren :: [LIEWrappedName GhcPs] -> [LIEWrappedName GhcPs]
    normaliseChildren newCs =
      let normalised = case newCs of
            []     -> []
            (c:cs) -> setEntryDP c (SameLine 0) : map (first ensureTrailingComma) cs
      in over _last (first removeTrailingCommaAnn) normalised

printExportList :: LExportList -> Text
printExportList l = T.pack (exactPrint (setEntryDP l (SameLine 0)))

-- | Exactprint a single item, without the surrounding list layout. The
-- trailing separator comma counts as layout: dropping it keeps a spliced item
-- from carrying a stray comma into text that already supplies its own.
printIE :: LIE GhcPs -> Text
printIE item = T.pack (exactPrint (setEntryDP (first removeTrailingCommaAnn item) (SameLine 0)))

-- | A fresh @T(ctor)@ export entry rendered as text, or 'Nothing' if @ctor@ is
-- already exported in the parsed list. Under CPP this adds a standalone entry so
-- the splice never reprints an existing @T(...)@ span, which can straddle a
-- directive.
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

-- | Decide how @ctor@ should be added under @parent@, classifying the first
-- matching export item by its constructor-carrying shape.
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
          _ | Just cs <- ieThingWithChildren ie ->
                if any ((== ctorFS) . lieWrappedNameFS) cs then AlreadyExported else AddChild
            | otherwise -> go rest
      | otherwise = go rest
