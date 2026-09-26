{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeData           #-}

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
-- data Hint = Hint { range :: Range, title :: Text }
--   deriving stock Generic
--   deriving anyclass 'Remappable'
-- @
--
-- A type without positions needs an 'Ageless' instance, to be read with
-- 'untrack' or 'ageless':
--
-- @
-- instance 'Ageless' HscEnvEq
-- @
--
-- An 'Ageless' field type can use 'Positionless':
--
-- @
-- deriving via 'Positionless' Severity instance 'Remappable' Severity
-- @
module Development.IDE.Core.Internal.Tracked
  ( Version
  , Aged
  , fromVersionOf
  , PositionMap
  , Tracked (..)
  , Remap
  , inFile
  , Remappable (..)
  , Positionless (..)
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
import           Control.Lens                         hiding (from, to)
import           Control.Monad
import           Data.Aeson
import           Data.Coerce
import           Data.Kind                            (Type)
import qualified Data.Map                             as Map
import           Data.Maybe
import qualified Data.Set                             as Set
import           Data.String
import           Data.Text                            (Text)
import           Development.IDE.Core.Internal.Fail
import qualified Development.IDE.Core.PositionMapping as P
import           Development.IDE.GHC.Compat           (DynFlags, Extension,
                                                       RealSrcSpan (..))
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.Error
import           Development.IDE.Types.Location
import           GHC.Generics
import           GHC.Utils.Outputable
import           Ide.Plugin.Error
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Types

type data Version

-- | A value of the version @s@ of a file.
newtype Aged (s :: Version) (a :: Type) = UnsafeAged
  { unsafeUnAge :: a
  }
  deriving stock (Functor, Foldable, Traversable)
  deriving newtype (Eq, Ord, Show, ToJSON, NFData)
  deriving Applicative via Identity

fromVersionOf :: Aged s a -> b -> Aged s b
fromVersionOf a b = b <$ a

-- | Maps version @s@ of a file to the current version.
data PositionMap (s :: Version) = PositionMap NormalizedFilePath P.PositionMapping

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
  default remap :: (Generic a, GRemappable (Rep a)) => Remap -> a -> Maybe a
  remap f = fmap to . gremap f . from

-- | A value fails to map if any of its fields fails.
class GRemappable f where
  gremap :: Remap -> f p -> Maybe (f p)

instance GRemappable V1 where
  gremap _ = Just

instance GRemappable U1 where
  gremap _ = Just

instance Remappable c => GRemappable (K1 i c) where
  gremap f (K1 c) = K1 <$> remap f c

instance GRemappable g => GRemappable (M1 i m g) where
  gremap f (M1 g) = M1 <$> gremap f g

instance (GRemappable g, GRemappable h) => GRemappable (g :*: h) where
  gremap f (g :*: h) = (:*:) <$> gremap f g <*> gremap f h

instance (GRemappable g, GRemappable h) => GRemappable (g :+: h) where
  gremap f (L1 g) = L1 <$> gremap f g
  gremap f (R1 h) = R1 <$> gremap f h

-- | For @deriving via@: an 'Ageless' type maps to itself.
newtype Positionless a = Positionless a

instance Ageless a => Remappable (Positionless a) where
  remap _ = Just

deriving via Positionless Bool instance Remappable Bool
deriving via Positionless Int instance Remappable Int
deriving via Positionless Text instance Remappable Text
deriving via Positionless Uri instance Remappable Uri

instance Remappable Position where
  remap (Remap _ move) = move
  remap Unchanged      = Just

instance Remappable Range where
  remap f (Range a b) = Range <$> remap f a <*> remap f b

instance Remappable RealSrcSpan where
  remap f sp = rangeToRealSrcSpan (fromString file) <$> remap (inFile (toNormalizedFilePath' file) f) (realSrcSpanToRange sp)
    where
      file = unpackFS $ srcSpanFile sp

instance Remappable Location where
  remap f loc = case uriToNormalizedFilePath (toNormalizedUri (loc ^. L.uri)) of
    Just file -> L.range (remap (inFile file f)) loc
    Nothing   -> Just loc

instance Remappable TextEdit
instance Remappable DocumentHighlightKind
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

instance Ageless Bool
instance Ageless DynFlags
instance Ageless Extension
instance Ageless Int
instance Ageless NamePprCtx
instance Ageless Text
instance Ageless Uri
instance Ageless a => Ageless [a]
instance Ageless a => Ageless (Maybe a)
instance (Ageless a, Ageless b) => Ageless (Either a b)
instance (Ageless a, Ageless b) => Ageless (a, b)
instance (Ageless k, Ageless v) => Ageless (Map.Map k v)
instance Ageless a => Ageless (Set.Set a)

ageless :: Ageless a => Aged s a -> a
ageless = coerce

untrack :: Ageless a => Tracked a -> a
untrack (Tracked a _) = ageless a

unsafeMkStale :: a -> Aged s a
unsafeMkStale = coerce
