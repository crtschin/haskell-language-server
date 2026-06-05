module UsesRefineExports where

import           RefineExports (T (..), used)

consumed :: Int
consumed = case MkT used of MkT n -> n
