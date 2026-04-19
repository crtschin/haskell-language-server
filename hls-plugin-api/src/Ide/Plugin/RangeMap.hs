{-# LANGUAGE DerivingStrategies #-}

-- | A map that allows fast \"in-range\" filtering. 'RangeMap' is meant
-- to be constructed once and cached as part of a Shake rule. If
-- not, the map will be rebuilt upon each invocation, yielding slower
-- results compared to the list-based approach!
module Ide.Plugin.RangeMap
  ( RangeMap(..),
    fromList,
    fromList',
    filterByRange,
    elementsInRange,
  ) where

import           Data.Bifunctor                (first)
import qualified Data.IntervalMap.FingerTree   as IM
import           Development.IDE.Graph.Classes (NFData)
import           Language.LSP.Protocol.Types   (Position, Range (Range))

-- | A map from code ranges to values.
newtype RangeMap a = RangeMap
  { unRangeMap :: IM.IntervalMap Position a
    -- ^ 'IM.Interval' of 'Position' corresponds to a 'Range'
  }
  deriving newtype (NFData, Semigroup, Monoid)
  deriving stock (Functor, Foldable, Traversable)

-- | Construct a 'RangeMap' from a 'Range' accessor and a list of values.
fromList :: (a -> Range) -> [a] -> RangeMap a
fromList extractRange = fromList' . map (\x -> (extractRange x, x))

fromList' :: [(Range, a)] -> RangeMap a
fromList' = RangeMap . toIntervalMap . map (first rangeToInterval)
  where
    toIntervalMap :: Ord v => [(IM.Interval v, a)] -> IM.IntervalMap v a
    toIntervalMap = foldl' (\m (i, v) -> IM.insert i v m) IM.empty

-- | Filter a 'RangeMap' by a given 'Range'.
filterByRange :: Range -> RangeMap a -> [a]
filterByRange range = map snd . IM.dominators (rangeToInterval range) . unRangeMap

-- | Extracts all elements from a 'RangeMap' that fall within a given 'Range'.
elementsInRange :: Range -> RangeMap a -> [a]
elementsInRange range = map snd . IM.intersections (rangeToInterval range) . unRangeMap

-- NOTE(ozkutuk): In itself, this conversion is wrong. As Michael put it:
-- "LSP Ranges have exclusive upper bounds, whereas the intervals here are
-- supposed to be closed (i.e. inclusive at both ends)"
-- However, in our use-case this turns out not to be an issue (supported
-- by the accompanying property test).  I think the reason for this is,
-- even if rangeToInterval isn't a correct 1:1 conversion by itself, it
-- is used for both the construction of the RangeMap and during the actual
-- filtering (filterByRange), so it still behaves identical to the list
-- approach.
-- This definition isn't exported from the module, therefore we need not
-- worry about other uses where it potentially makes a difference.
rangeToInterval :: Range -> IM.Interval Position
rangeToInterval (Range s e) = IM.Interval s e
