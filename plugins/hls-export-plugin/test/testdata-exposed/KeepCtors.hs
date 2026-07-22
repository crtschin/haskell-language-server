module KeepCtors (Wrap (Wrap), N (N), D (MkD), unused) where

newtype Wrap = Wrap Int

instance Semigroup Wrap where
  a <> _ = a

newtype N = N Int

data D = MkD Int

unused :: Int
unused = 1
