{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.Export.ExactPrint
  ( LExportList
  , mkValueIE
  , mkTypeAllIE
  , availToLIE
  , mkExportList
  , appendIE
  , removeNamedIE
  , reconcileExportList
  , addCtorUnderParent
  , removeCtorUnderParent
  , printExportList
  , toDeltaExportList
  , isInIE
  ) where

import           Control.Lens                    (_last, over)
import           Data.Bifunctor                  (first)
import           Data.List                       (foldl', mapAccumL)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NE
import           Data.Maybe                      (fromMaybe, listToMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Orphans     ()
#if MIN_VERSION_ghc(9,11,0)
import           GHC                             (AnnList (..), DeltaPos (..),
                                                  EpAnn (..), EpaLocation (..),
                                                  EpaLocation' (..), LocatedL,
                                                  NoAnn (..), SrcSpanAnnA,
                                                  TrailingAnn (..),
                                                  emptyComments, noAnn)
#elif MIN_VERSION_ghc(9,9,0)
import           GHC                             (AnnList (..), DeltaPos (..),
                                                  EpAnn (..), EpaLocation,
                                                  EpaLocation' (..), LocatedL,
                                                  NoAnn (..), SrcSpanAnnA,
                                                  TrailingAnn (..),
                                                  emptyComments, noAnn)
#else
import           GHC                             (AnnList (..), DeltaPos (..),
                                                  EpAnn (..), EpaLocation (..),
                                                  LocatedL, SrcSpanAnn' (..),
                                                  SrcSpanAnnA, TrailingAnn (..),
                                                  addAnns, emptyComments, noAnn)
#endif

#if !MIN_VERSION_ghc(9,9,0)
import           GHC.Parser.Annotation           (Anchor (..),
                                                  AnchorOperation (..),
                                                  placeholderRealSpan)
#endif
import           Language.Haskell.GHC.ExactPrint (addComma, exactPrint,
                                                  makeDeltaAst, setEntryDP)

#if MIN_VERSION_ghc(9,11,0)
import           GHC                             (AnnListBrackets (..),
                                                  EpToken (..), LocatedLI)
import           GHC.Types.SrcLoc                (UnhelpfulSpanReason (..))
#else
import           GHC                             (AddEpAnn (..))
#endif
import           Development.IDE.GHC.Compat.Util
import           Ide.Plugin.Export.Utils

-- | Located @[LIE GhcPs]@ — the shape of an export list. Aliases either
-- 'LocatedL' (pre-9.12) or 'LocatedLI' (9.12+ added a "hiding" slot to the
-- list annotation).
#if MIN_VERSION_ghc(9,11,0)
type LExportList = LocatedLI [LIE GhcPs]
#else
type LExportList = LocatedL [LIE GhcPs]
#endif

availToLIE :: AvailInfo -> Maybe (LIE GhcPs)
availToLIE = \case
  AvailName n -> Just (mkValueIE (nameRdr n))
  AvailTC parent names _
    | all (== parent) names -> Just (mkTypeAbsIE (nameRdr parent))
    | otherwise -> Just (mkTypeAllIE (nameRdr parent))
  AvailFL _ -> Nothing
  where
    nameRdr = mkRdrUnqual . nameOccName

-- | @foo@
mkValueIE :: RdrName -> LIE GhcPs
mkValueIE rdr =
  reLocA $ L noSrcSpan $ IEVar
#if MIN_VERSION_ghc(9,8,0)
    Nothing
#else
    noExtField
#endif
    (mkIEName rdr)
#if MIN_VERSION_ghc(9,9,0)
    Nothing
#endif

-- | Bare @T@ (no constructors listed).
mkTypeAbsIE :: RdrName -> LIE GhcPs
mkTypeAbsIE rdr =
  reLocA $ L noSrcSpan $ IEThingAbs
#if MIN_VERSION_ghc(9,11,0)
    Nothing
#elif MIN_VERSION_ghc(9,8,0)
    (Nothing, noAnn)
#else
    noAnn
#endif
    (mkIEName rdr)
#if MIN_VERSION_ghc(9,9,0)
    Nothing
#endif

-- | @T(..)@
mkTypeAllIE :: RdrName -> LIE GhcPs
mkTypeAllIE rdr =
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
    (mkIEName rdr)
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
    children = case NE.toList ctors of
      []     -> [] -- impossible
      (c:cs) -> mkIEName c : map (\x -> first addComma (mkIEName x)) cs

-- | Build a fresh located @(item1, item2, ...)@ from a list of items.
mkExportList :: [LIE GhcPs] -> LExportList
mkExportList items =
#if MIN_VERSION_ghc(9,9,0)
  L (EpAnn (entryAnchor (SameLine 1)) listAnn emptyComments) (commaList items)
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

-- | Internal: wrap an 'RdrName' as an 'IEName' located node.
mkIEName :: RdrName -> LIEWrappedName GhcPs
mkIEName rdr =
  reLocA $ L noSrcSpan $ IEName
#if MIN_VERSION_ghc(9,5,0)
    noExtField
#endif
    (reLocA (L noSrcSpan rdr))

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
#if MIN_VERSION_ghc(9,9,0)
separatorComma items =
  listToMaybe [c | L ann _ <- items, c <- trailingAnns ann, isCommaAnn c]
#else
separatorComma _ = Nothing
#endif

removeNamedIE :: RdrName -> LExportList -> Maybe LExportList
removeNamedIE name (L l items) = case break matches items of
  (_, []) -> Nothing
  (pre, _ : post) ->
    let kept = pre ++ post
        kept' = over _last (first removeTrailingCommaAnn) kept
        kept'' = if null pre then resetFirstEntryDP kept' else kept'
    in Just (L l kept'')
  where
    fs = rdrNameFS name
    matches (L _ ie) = maybe False ((== fs) . rdrNameFS) (ieParentName ie)
    resetFirstEntryDP []     = []
    resetFirstEntryDP (x:xs) = setEntryDP x (SameLine 0) : xs

-- | Rewrite an existing export list to contain exactly @desired@ (matched by parent
-- name), keeping surviving items' layout. A matched item takes the desired entry's
-- shape, so a stale @T(C1)@ becomes the wanted @T(..)@ or @T@.
reconcileExportList :: [LIE GhcPs] -> LExportList -> LExportList
reconcileExportList desired lst@(L _ items0) =
    foldl' (flip appendIE) (overItems (map swap) trimmed) additions
  where
    desiredByFS = [(rdrNameFS p, ie) | L _ ie <- desired, Just p <- [ieParentName ie]]
    desiredFSs  = map fst desiredByFS
    existingFSs = [rdrNameFS p | L _ ie <- items0, Just p <- [ieParentName ie]]

    staleNames = [p | L _ ie <- items0, Just p <- [ieParentName ie], rdrNameFS p `notElem` desiredFSs]
    additions  = [item | item@(L _ ie) <- desired, Just p <- [ieParentName ie], rdrNameFS p `notElem` existingFSs]

    trimmed = foldl' (\acc n -> fromMaybe acc (removeNamedIE n acc)) lst staleNames

    swap (L ann ie) = case ieParentName ie of
      Just p | Just newIe <- lookup (rdrNameFS p) desiredByFS -> L ann newIe
      _                                                       -> L ann ie

    overItems f (L l items) = L l (f items)

-- | 'Nothing' iff @ctor@ is already exported (via @T(..)@ or @T(...,ctor,...)@).
addCtorUnderParent ::
  RdrName {- ^ parent -} ->
  RdrName {- ^ ctor -} ->
  LExportList ->
  Maybe LExportList
addCtorUnderParent parent ctor lst@(L l items) =
  case findParent items of
    ParentNotFound -> Just $ appendIE (mkTypeWithIE parent (ctor :| [])) lst
    FoundIEThingAll -> Nothing
    FoundIEThingWith CtorPresent -> Nothing
    FoundIEThingWith CtorAbsent -> Just (L l (map (transformParent extendThingWith) items))
    FoundIEThingAbs ->
      let upgraded = unLoc (mkTypeWithIE parent (ctor :| []))
      in Just (L l (map (transformParent (const upgraded)) items))
  where
    parentFS = rdrNameFS parent
    ctorFS = rdrNameFS ctor

    ctorPresence cs
      | any ((== ctorFS) . rdrNameFS . ieWrappedRdrName . unLoc) cs = CtorPresent
      | otherwise = CtorAbsent

    findParent [] = ParentNotFound
    findParent (L _ ie : rest)
      | maybe False ((== parentFS) . rdrNameFS) (ieParentName ie) =
          case ie of
            IEThingAll{} -> FoundIEThingAll
            IEThingAbs{} -> FoundIEThingAbs
            _ | Just cs <- ieThingWithChildren ie -> FoundIEThingWith (ctorPresence cs)
              | otherwise                         -> findParent rest
      | otherwise = findParent rest

    transformParent f (L itemLoc ie)
      | maybe False ((== parentFS) . rdrNameFS) (ieParentName ie) = L itemLoc (f ie)
      | otherwise = L itemLoc ie

    extendThingWith :: IE GhcPs -> IE GhcPs
    extendThingWith = overThingWithChildren $ \cs ->
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
      | maybe False ((== parentFS) . rdrNameFS) (ieParentName ie)
      , Just cs <- ieThingWithChildren ie
      , any ((== ctorFS) . rdrNameFS . ieWrappedRdrName . unLoc) cs =
          let cs' = filter ((/= ctorFS) . rdrNameFS . ieWrappedRdrName . unLoc) cs
              ie' = case cs' of
                      [] -> downgradeToAbs ie
                      _  -> overThingWithChildren (const (normaliseChildren cs')) ie
          in (True, L itemLoc ie')
      | otherwise = (acc, lie)

    downgradeToAbs :: IE GhcPs -> IE GhcPs
    downgradeToAbs ie
      | Just _ <- ieThingWithChildren ie = unLoc (mkTypeAbsIE parent)
      | otherwise                        = ie

    normaliseChildren :: [LIEWrappedName GhcPs] -> [LIEWrappedName GhcPs]
    normaliseChildren newCs =
      let normalised = case newCs of
            []     -> []
            (c:cs) -> setEntryDP c (SameLine 0) : map (first ensureTrailingComma) cs
      in over _last (first removeTrailingCommaAnn) normalised

printExportList :: LExportList -> Text
printExportList l = T.pack (exactPrint (setEntryDP l (SameLine 0)))

toDeltaExportList :: LExportList -> LExportList
toDeltaExportList = makeDeltaAst

ensureTrailingComma :: SrcSpanAnnA -> SrcSpanAnnA
ensureTrailingComma ann
  | any isCommaAnn (trailingAnns ann) = ann
  | otherwise = addComma ann

trailingAnns :: SrcSpanAnnA -> [TrailingAnn]
#if MIN_VERSION_ghc(9,9,0)
trailingAnns (EpAnn _ (AnnListItem as) _) = as
#else
trailingAnns sa = case ann sa of
  EpAnn _ (AnnListItem as) _ -> as
  _                          -> []
#endif

-- | Map over an item's trailing annotations, hiding the version-specific 'AnnListItem' shape.
overTrailingAnns :: ([TrailingAnn] -> [TrailingAnn]) -> SrcSpanAnnA -> SrcSpanAnnA
#if MIN_VERSION_ghc(9,9,0)
overTrailingAnns f (EpAnn anc (AnnListItem as) cs) = EpAnn anc (AnnListItem (f as)) cs
#else
overTrailingAnns _ it@(SrcSpanAnn EpAnnNotUsed _) = it
overTrailingAnns f (SrcSpanAnn (EpAnn anc (AnnListItem as) cs) l) =
  SrcSpanAnn (EpAnn anc (AnnListItem (f as)) cs) l
#endif

removeTrailingCommaAnn :: SrcSpanAnnA -> SrcSpanAnnA
removeTrailingCommaAnn = overTrailingAnns (filter (not . isCommaAnn))

-- | Replace an item's trailing comma with @c@, preserving its delta.
withTrailingComma :: TrailingAnn -> SrcSpanAnnA -> SrcSpanAnnA
withTrailingComma c = overTrailingAnns (\as -> filter (not . isCommaAnn) as ++ [c])

isCommaAnn :: TrailingAnn -> Bool
isCommaAnn AddCommaAnn{} = True
isCommaAnn _             = False

#if MIN_VERSION_ghc(9,9,0)
entryAnchor :: DeltaPos -> EpaLocation
#if MIN_VERSION_ghc(9,11,0)
entryAnchor dp = EpaDelta (UnhelpfulSpan UnhelpfulNoLocationInfo) dp []
#else
entryAnchor dp = EpaDelta dp []
#endif
#endif

epl :: Int -> EpaLocation
#if MIN_VERSION_ghc(9,11,0)
epl n = EpaDelta (UnhelpfulSpan UnhelpfulNoLocationInfo) (SameLine n) []
#else
epl n = EpaDelta (SameLine n) []
#endif

data FindParentResult
  = ParentNotFound
  | FoundIEThingAll
  | FoundIEThingWith CtorPresence
  | FoundIEThingAbs

data CtorPresence = CtorAbsent | CtorPresent
  deriving Eq

isInIE :: FastString -> IE GhcPs -> Bool
isInIE n =
  maybe False (any ((== n) . rdrNameFS . ieWrappedRdrName . unLoc)) . ieThingWithChildren
