module UsesMakeExplicitUsed where

import           MakeExplicitUsed (T (..), usedByOther)

consumed :: Int
consumed = case MkT usedByOther of MkT n -> n
