{-# LANGUAGE CPP #-}
module TrimExports
  ( -- * Core
    used
  , unused
  , T (MkA, MkB)
  , Q (QA)
  , UnusedT (..)
  , Wrap (Wrap), N (N), D (MkD)
  , Clash, pattern Clash
  , originUsed, originUnused
  , ReexportOrigin.qualUsed
  , fieldUsed
  , localUnused
  ) where

import qualified ReexportOrigin
import           ReexportOrigin (fieldUsed, originUnused, originUsed)

#ifdef NOPE
extra :: Int
extra = 0
#endif

used :: Int
used = 1

unused :: Int
unused = 2

localUnused :: Int
localUnused = 3

data T = MkA | MkB

data Q = QA | QB | QC

data UnusedT = MkUnusedT

newtype Wrap = Wrap Int

instance Semigroup Wrap where
  a <> _ = a

newtype N = N Int

data D = MkD Int

data Clash = MkClash

pattern Clash :: Clash
pattern Clash = MkClash
