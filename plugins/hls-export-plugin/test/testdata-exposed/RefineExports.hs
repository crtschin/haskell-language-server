module RefineExports (used, unused, T (..), UnusedT (..)) where

used :: Int
used = 1

unused :: Int
unused = 2

data T = MkT Int
data UnusedT = MkUnusedT
