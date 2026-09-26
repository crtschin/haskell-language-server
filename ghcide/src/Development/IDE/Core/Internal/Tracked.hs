{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE PatternSynonyms   #-}

-- | Positions of last known values.
--
-- A last known value can come from an older version of its file, and its
-- positions translate between that version and the current one.
module Development.IDE.Core.Internal.Tracked
  ( -- * Tracked values
    Tracked (..)
  , rewind
  , rewindClamped
  , fastForward
  , fastForwardEach
  , answerAt
  , untrack
  , untrackStale
  , tracked
  , untracked
    -- * Translation
  , Remap
  , Remappable (..)
  , Positionless (..)
  , Ageless
  ) where

import           Control.Monad
import           Data.List.NonEmpty                   (NonEmpty)
import qualified Data.Map                             as Map
import           Data.Maybe
import           Data.Proxy                           (Proxy)
import qualified Data.Set                             as Set
import           Data.String
import           Data.Text                            (Text)
import qualified Development.IDE.Core.PositionMapping as P
import           Development.IDE.GHC.Compat           (DynFlags, Extension,
                                                       Fixity, Module,
                                                       ModuleName, Name,
                                                       OccName, RealSrcSpan,
                                                       SrcSpan (UnhelpfulSpan),
                                                       UniqFM, nameSrcSpan,
                                                       pattern RealSrcSpan,
                                                       setNameLoc, srcSpanFile)
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.Error
import           Development.IDE.Types.Location
import           Generics.OneLiner                    (ADT, Constraints,
                                                       gtraverse)
import           GHC.Types.SrcLoc                     (UnhelpfulSpanReason (UnhelpfulOther))
import           GHC.Utils.Outputable                 (NamePprCtx)
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Types

-- | A last known value, with the mapping from its file's version to the
-- current one.
data Tracked a = Tracked a NormalizedFilePath P.PositionMapping
  deriving stock (Functor, Foldable, Traversable)

tracked :: NormalizedFilePath -> (a, P.PositionMapping) -> Tracked a
tracked file (a, pm) = Tracked a file pm

untracked :: Tracked a -> (a, P.PositionMapping)
untracked (Tracked a _ pm) = (a, pm)

-- | Rewinds a position of the request into the version of the value.
-- 'Nothing' when the user edited that spot since.
--
-- Use this when you fast-forward the answer yourself, e.g. with
-- 'fastForwardEach'.
rewind :: Remappable p => (a -> p -> b) -> p -> Tracked a -> Maybe (Tracked b)
rewind f p (Tracked a file pm) = (\p' -> Tracked (f a p') file pm) <$> remap (Remap Rewind file pm) p

-- | Like 'rewind', but where the user edited the spot, the start of that edit.
rewindClamped :: (a -> Position -> b) -> Position -> Tracked a -> Tracked b
rewindClamped f p t@(Tracked _ _ (P.PositionMapping delta)) =
  (`f` P.lowerRange (P.fromDelta delta p)) <$> t

-- | Fast-forwards the value to the current version. 'Nothing' when an edit
-- invalidated any of its positions.
fastForward :: Remappable a => Tracked a -> Maybe a
fastForward (Tracked a file pm) = remap (Remap FastForward file pm) a

-- | Fast-forwards each element on its own. Drops the ones that an edit
-- invalidated.
--
-- Use this when one stale element shouldn't drop the rest, e.g. for semantic
-- tokens.
fastForwardEach :: Remappable a => Tracked [a] -> [a]
fastForwardEach = mapMaybe fastForward . sequenceA

-- | Like 'rewind', but fast-forwards the answer to the current version.
--
-- Use this to answer at the cursor, e.g. for a hover or find-references.
answerAt :: (Remappable p, Remappable b) => (a -> p -> b) -> p -> Tracked a -> Maybe b
answerAt f p = fastForward <=< rewind f p

-- | Use this for a value without positions, e.g. a session or a docs map.
untrack :: Ageless a => Tracked a -> a
untrack = untrackStale

-- | Reads a value with its positions in its own version.
untrackStale :: Tracked a -> a
untrackStale (Tracked a _ _) = a

data Direction = Rewind | FastForward

-- | Translates the positions of one file. A position of another file keeps its
-- place, as the mapping has no version of that file.
data Remap = Remap Direction NormalizedFilePath P.PositionMapping

inFileOf :: Remap -> Uri -> Bool
inFileOf (Remap _ file _) uri = uriToNormalizedFilePath (toNormalizedUri uri) == Just file

-- | A type whose positions translate between versions of a file. A bare
-- 'Position' or 'Range' belongs to the file of the translation.
class Remappable a where
  remap :: Remap -> a -> Maybe a
  default remap :: (ADT a, Constraints a Remappable) => Remap -> a -> Maybe a
  remap f = gtraverse @Remappable (remap f)

-- | Vouches that a type has no positions, for @deriving via@.
newtype Positionless a = Positionless a

instance Remappable (Positionless a) where
  remap _ = Just

instance Ageless (Positionless a) where
  vouch _ = ()

deriving via Positionless () instance Remappable ()
deriving via Positionless Bool instance Remappable Bool
deriving via Positionless Char instance Remappable Char
deriving via Positionless Int instance Remappable Int
deriving via Positionless Int32 instance Remappable Int32
deriving via Positionless UInt instance Remappable UInt
deriving via Positionless Text instance Remappable Text
deriving via Positionless Null instance Remappable Null
deriving via Positionless Uri instance Remappable Uri
deriving via Positionless MarkupContent instance Remappable MarkupContent
deriving via Positionless ModuleName instance Remappable ModuleName

instance Remappable Position where
  remap (Remap Rewind _ m)      = P.fromCurrentPosition m
  remap (Remap FastForward _ m) = P.toCurrentPosition m

instance Remappable RealSrcSpan where
  remap f@(Remap _ file _) sp
    | toNormalizedFilePath' spanFile == file =
        rangeToRealSrcSpan (fromString spanFile) <$> remap f (realSrcSpanToRange sp)
    | otherwise = Just sp
    where
      spanFile = unpackFS $ srcSpanFile sp

-- | Never fails. When the user edits a name's definition, the name only loses its
-- span, its unique still works as a key.
instance Remappable Name where
  remap f@(Remap _ file _) n = Just $ case nameSrcSpan n of
    RealSrcSpan sp _
      | toNormalizedFilePath' (unpackFS (srcSpanFile sp)) == file ->
          setNameLoc n $ maybe edited (\sp' -> RealSrcSpan sp' Nothing) (remap f sp)
    _ -> n
    where
      edited = UnhelpfulSpan (UnhelpfulOther (fsLit "<edited since>"))

instance Remappable Location where
  remap f l@(Location uri range)
    | inFileOf f uri = Location uri <$> remap f range
    | otherwise = Just l

-- | A link translates its target only when the target is in the file of the
-- translation.
instance Remappable LocationLink where
  remap f (LocationLink origin uri target selection) =
    LocationLink <$> remap f origin <*> pure uri <*> remapTarget target <*> remapTarget selection
    where
      remapTarget r
        | inFileOf f uri = remap f r
        | otherwise = Just r

-- | A hover keeps its contents when its range fails.
instance Remappable Hover where
  remap f (Hover contents range) = Just (Hover contents (remap f =<< range))

-- | A symbol drops the children that fail.
instance Remappable DocumentSymbol where
  remap f =
    L.range (remap f)
      >=> L.selectionRange (remap f)
      >=> L.children (Just . fmap (mapMaybe (remap f)))

instance Remappable Range
instance Remappable TextEdit
instance Remappable DocumentHighlightKind
instance Remappable DocumentHighlight
instance Remappable SelectionRange

instance Remappable a => Remappable [a] where
  remap = traverse . remap

instance Remappable a => Remappable (NonEmpty a)
instance Remappable a => Remappable (Maybe a)
instance (Remappable a, Remappable b) => Remappable (Either a b)
instance (Remappable a, Remappable b) => Remappable (a |? b)
instance (Remappable a, Remappable b) => Remappable (a, b)
instance (Remappable a, Remappable b, Remappable c) => Remappable (a, b, c)

-- | A type without positions, which can leave its version.
class Ageless a where
  -- | Never called. Defining it vouches for the type.
  vouch :: Proxy a -> ()
  default vouch :: Constraints a Ageless => Proxy a -> ()
  vouch _ = ()

deriving via Positionless () instance Ageless ()
deriving via Positionless Bool instance Ageless Bool
deriving via Positionless Char instance Ageless Char
deriving via Positionless Int instance Ageless Int
deriving via Positionless Int32 instance Ageless Int32
deriving via Positionless UInt instance Ageless UInt
deriving via Positionless Text instance Ageless Text
deriving via Positionless Null instance Ageless Null
deriving via Positionless Uri instance Ageless Uri
deriving via Positionless NormalizedFilePath instance Ageless NormalizedFilePath
deriving via Positionless ModuleName instance Ageless ModuleName
deriving via Positionless DynFlags instance Ageless DynFlags
deriving via Positionless Extension instance Ageless Extension
deriving via Positionless NamePprCtx instance Ageless NamePprCtx

instance Ageless a => Ageless [a]
instance Ageless a => Ageless (NonEmpty a)
instance Ageless a => Ageless (Maybe a)
instance (Ageless a, Ageless b) => Ageless (Either a b)
instance (Ageless a, Ageless b) => Ageless (a, b)
instance (Ageless a, Ageless b, Ageless c) => Ageless (a, b, c)
deriving via Positionless (Map.Map k v) instance (Ageless k, Ageless v) => Ageless (Map.Map k v)
deriving via Positionless (Set.Set a) instance Ageless a => Ageless (Set.Set a)
-- | Holds the uniques of its keys only.
deriving via Positionless (UniqFM k a) instance Ageless a => Ageless (UniqFM k a)
deriving via Positionless OccName instance Ageless OccName
deriving via Positionless Module instance Ageless Module
deriving via Positionless Fixity instance Ageless Fixity
