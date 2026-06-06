module ExpandExports (sort, alreadyListed) where

import           Data.List (sort)

alreadyListed :: Int
alreadyListed = 1

notYetListed :: Int
notYetListed = 2

data Extra = Extra Int
