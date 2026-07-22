module UsesMultilineRefine where

import           MultilineRefine (T (..), used)

consumed :: Int
consumed = case MkT used of MkT n -> n
