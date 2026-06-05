module MultilineRefine
  ( used
  , unused
  , T (..)
  ) where

used :: Int
used = 1

unused :: Int
unused = 2

data T = MkT Int
