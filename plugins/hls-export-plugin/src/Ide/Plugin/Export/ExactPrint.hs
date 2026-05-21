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
  , addCtorUnderParent
  , removeCtorUnderParent
  , printExportList
  , toDeltaExportList
  , isInIE
  ) where

import           Control.Lens                    (_last, over)
import           Data.Bifunctor                  (first)
import           Data.List                       (mapAccumL)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NE
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
    | null (filter (/= parent) names) -> Just (mkTypeAbsIE (nameRdr parent))
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

    commaList []     = []
    commaList [x]    = [setEntryDP (first removeTrailingCommaAnn x) (SameLine 0)]
    commaList (x:xs) = setEntryDP (first ensureTrailingComma x) (SameLine 0) : go xs
      where
        go [y]    = [setEntryDP (first removeTrailingCommaAnn y) (SameLine 1)]
        go (y:ys) = setEntryDP (first ensureTrailingComma y) (SameLine 1) : go ys
        go []     = []

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
    fixLast = over _last (first ensureTrailingComma)

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
            IEThingAll{}           -> FoundIEThingAll
#if MIN_VERSION_ghc(9,9,0)
            IEThingWith _ _ _ cs _ -> FoundIEThingWith (ctorPresence cs)
#else
            IEThingWith _ _ _ cs   -> FoundIEThingWith (ctorPresence cs)
#endif
            IEThingAbs{}           -> FoundIEThingAbs
            _                      -> findParent rest
      | otherwise = findParent rest

    transformParent f (L itemLoc ie)
      | maybe False ((== parentFS) . rdrNameFS) (ieParentName ie) = L itemLoc (f ie)
      | otherwise = L itemLoc ie

    extendThingWith :: IE GhcPs -> IE GhcPs
    extendThingWith (IEThingWith ann lname wild cs
#if MIN_VERSION_ghc(9,9,0)
                                  docs
#endif
                    ) =
      let hasSibling = not (null cs)
          newChild = setEntryDP (mkIEName ctor) (SameLine (if hasSibling then 1 else 0))
          cs' = (if hasSibling then map (first ensureTrailingComma) cs else cs) ++ [newChild]
      in IEThingWith ann lname wild cs'
#if MIN_VERSION_ghc(9,9,0)
                     docs
#endif
    extendThingWith other = other

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
      | maybe False ((== parentFS) . rdrNameFS) (ieParentName ie) =
          case ie of
#if MIN_VERSION_ghc(9,9,0)
            IEThingWith _ _ _ cs _
#else
            IEThingWith _ _ _ cs
#endif
              | any ((== ctorFS) . rdrNameFS . ieWrappedRdrName . unLoc) cs ->
                  let cs' = filter ((/= ctorFS) . rdrNameFS . ieWrappedRdrName . unLoc) cs
                      ie' = case cs' of
                              [] -> downgradeToAbs ie
                              _  -> rebuildThingWith ie cs'
                  in (True, L itemLoc ie')
            _ -> (acc, lie)
      | otherwise = (acc, lie)

    downgradeToAbs :: IE GhcPs -> IE GhcPs
    downgradeToAbs (IEThingWith _ lname _ _
#if MIN_VERSION_ghc(9,9,0)
                                docs
#endif
                   ) =
      IEThingAbs
#if MIN_VERSION_ghc(9,11,0)
        Nothing
#elif MIN_VERSION_ghc(9,8,0)
        (Nothing, noAnn)
#else
        noAnn
#endif
        lname
#if MIN_VERSION_ghc(9,9,0)
        docs
#endif
    downgradeToAbs other = other

    rebuildThingWith :: IE GhcPs -> [LIEWrappedName GhcPs] -> IE GhcPs
    rebuildThingWith (IEThingWith ann lname wild _
#if MIN_VERSION_ghc(9,9,0)
                                   docs
#endif
                     ) newCs =
      let normalised = case newCs of
            []     -> []
            (c:cs) -> setEntryDP c (SameLine 0) : map (first ensureTrailingComma) cs
          stripped = over _last (first removeTrailingCommaAnn) normalised
      in IEThingWith ann lname wild stripped
#if MIN_VERSION_ghc(9,9,0)
                     docs
#endif
    rebuildThingWith other _ = other

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

removeTrailingCommaAnn :: SrcSpanAnnA -> SrcSpanAnnA
#if MIN_VERSION_ghc(9,9,0)
removeTrailingCommaAnn (EpAnn anc (AnnListItem as) cs) =
  EpAnn anc (AnnListItem (filter (not . isCommaAnn) as)) cs
#else
removeTrailingCommaAnn it@(SrcSpanAnn EpAnnNotUsed _) = it
removeTrailingCommaAnn (SrcSpanAnn (EpAnn anc (AnnListItem as) cs) l) =
  SrcSpanAnn (EpAnn anc (AnnListItem (filter (not . isCommaAnn) as)) cs) l
#endif

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
isInIE n ie = case ie of
#if MIN_VERSION_ghc(9,9,0)
  IEThingWith _ _ _ cs _ ->
    any ((== n) . rdrNameFS . ieWrappedRdrName . unLoc) cs
#else
  IEThingWith _ _ _ cs   ->
    any ((== n) . rdrNameFS . ieWrappedRdrName . unLoc) cs
#endif
  _ -> False
