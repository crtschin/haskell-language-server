{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE GADTs             #-}

-- | Positions of last known values.
--
-- A last known value can come from an older version of files. An @'Aged' s a@
-- is a value of version @s@.
--
-- @
--   rewind        request     ->  version s
--   fastForward   version s   ->  request
-- @
--
-- == Examples
--
-- Values of one version combine with 'fmap' and '<*>':
--
-- @
-- 'Tracked' ast mapping <- 'settle_' GetHieAst file
-- oldPos <- 'rewind' mapping pos
-- 'fastForward' mapping (documentHighlights \<$\> ast \<*\> oldPos)
-- @
--
-- With 'fromVersionOf', a value can use the version from a reference, e.g. the
-- location of a name in the AST, to apply a mapping:
--
-- @
-- loc <- 'fastForward' mapping ('fromVersionOf' ast (srcSpanToLocation (nameSrcSpan name)))
-- @
--
-- With 'fastForwardEach', a handler can keep elements that haven't been
-- invalidated due to edits:
--
-- @
-- let children = 'fastForwardEach' mapping (documentSymbols \<$\> parsed)
-- @
--
-- With 'rewindBounds', a handler gets a position for the cursor even inside an
-- edit, e.g. for completion:
--
-- @
-- let (lower, upper) = 'rewindBounds' mapping cursor
-- @
--
-- A type with positions needs a 'Remappable' instance:
--
-- @
-- instance 'Remappable' TextEdit
-- @
--
-- A type without positions needs an 'Ageless' instance, to be read with
-- 'untrack' or 'ageless':
--
-- @
-- instance 'Ageless' HscEnvEq
-- @
module Development.IDE.Core.Internal.Tracked
  ( Aged
  , fromVersionOf
  , PositionMap
  , Tracked (..)
  , Remap
  , inFile
  , Remappable (..)
  , rewind
  , fastForward
  , fastForwardEach
  , rewindBounds
  , Ageless
  , ageless
  , untrack
    -- * Conversions for the query API
  , tracked
  , untracked
    -- * Escape hatches
  , unsafeUnAge
  , unsafeMkStale
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Coerce
import           Data.Maybe
import           Data.String
import           Development.IDE.Core.Internal.Fail
import qualified Development.IDE.Core.PositionMapping as P
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.Error
import           Development.IDE.Types.Location
import           GHC.Utils.Outputable
import           Ide.Plugin.Error
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Types

-- | A value of the version @s@ of a file.
newtype Aged s a = UnsafeAged
  { unsafeUnAge :: a
  }
  deriving stock (Functor, Foldable, Traversable)
  deriving newtype (Eq, Ord, Show, ToJSON, NFData)
  deriving Applicative via Identity

fromVersionOf :: Aged s a -> b -> Aged s b
fromVersionOf a b = b <$ a

-- | Maps version @s@ of a file to the current version.
data PositionMap s = PositionMap NormalizedFilePath P.PositionMapping

data Tracked a where
  Tracked :: Aged s a -> PositionMap s -> Tracked a

instance Functor Tracked where
  fmap f (Tracked t pm) = Tracked (fmap f t) pm

tracked :: NormalizedFilePath -> (a, P.PositionMapping) -> Tracked a
tracked file (a, pm) = Tracked (coerce a) (PositionMap file pm)

untracked :: Tracked a -> (a, P.PositionMapping)
untracked (Tracked ta (PositionMap _ pm)) = (coerce ta, pm)

data Remap
  = Remap NormalizedFilePath (Position -> Maybe Position)
  | Unchanged

-- | Only apply remappings where the file match.
inFile :: NormalizedFilePath -> Remap -> Remap
inFile file r@(Remap mapFile _) | file == mapFile = r
inFile _ _ = Unchanged

class Remappable a where
  remap :: Remap -> a -> Maybe a
  default remap :: (L.HasRange a Range) => Remap -> a -> Maybe a
  remap = L.range . remap

instance Remappable Position where
  remap (Remap _ move) = move
  remap Unchanged = Just

instance Remappable Range where
  remap f (Range a b) = Range <$> remap f a <*> remap f b

instance Remappable RealSrcSpan where
  remap f sp = rangeToRealSrcSpan (fromString file) <$> remap (inFile (toNormalizedFilePath' file) f) (realSrcSpanToRange sp)
    where
      file = unpackFS $ srcSpanFile sp

instance Remappable Location where
  remap f loc = case uriToNormalizedFilePath (toNormalizedUri (loc ^. L.uri)) of
    Just file -> L.range (remap (inFile file f)) loc
    Nothing -> Just loc

instance Remappable TextEdit
instance Remappable DocumentHighlight

instance Remappable DocumentSymbol where
  remap f =
    L.range (remap f)
      >=> L.selectionRange (remap f)
      >=> L.children (Just . fmap (mapMaybe (remap f)))

instance Remappable SelectionRange where
  remap f (SelectionRange r p) = SelectionRange <$> remap f r <*> Just (p >>= remap f)

instance (Remappable a, Ageless b) => Remappable (a, b) where
  remap f (a, b) = (,b) <$> remap f a

instance (Remappable a) => Remappable [a] where
  remap = traverse . remap

instance (Remappable a) => Remappable (Maybe a) where
  remap = traverse . remap

-- | Rewinds a position of the request, e.g. the cursor, into a last known value.
rewind :: (Remappable a, MonadPluginFail m) => PositionMap s -> a -> m (Aged s a)
rewind (PositionMap file m) =
  required (PluginInvalidUserState "rewind") . fmap UnsafeAged . remap (Remap file (P.fromCurrentPosition m))

-- | Fast-forwards a result from a last known value to the current version.
fastForward :: (Remappable a, MonadPluginFail m) => PositionMap s -> Aged s a -> m a
fastForward (PositionMap file m) (UnsafeAged a) =
  required (PluginInvalidUserState "fastForward") $ remap (Remap file (P.toCurrentPosition m)) a

fastForwardEach :: Remappable a => PositionMap s -> Aged s [a] -> [a]
fastForwardEach pm = mapMaybe (fastForward pm) . sequenceA

-- | Does the same as 'rewind', but gives the bounds of the edit instead of failing.
rewindBounds :: PositionMap s -> Position -> (Aged s Position, Aged s Position)
rewindBounds (PositionMap _ (P.PositionMapping delta)) pos =
  let r = P.fromDelta delta pos
   in (UnsafeAged (P.lowerRange r), UnsafeAged (P.upperRange r))

-- | A type without positions.
class Ageless a

instance Ageless DynFlags
instance Ageless Extension
instance Ageless NamePprCtx
instance Ageless Uri
instance Ageless a => Ageless [a]

ageless :: Ageless a => Aged s a -> a
ageless = coerce

untrack :: Ageless a => Tracked a -> a
untrack (Tracked a _) = ageless a

unsafeMkStale :: a -> Aged s a
unsafeMkStale = coerce
