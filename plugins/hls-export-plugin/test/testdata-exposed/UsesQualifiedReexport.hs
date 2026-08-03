module UsesQualifiedReexport where

import           QualifiedReexport (originUsed)

consumed :: Int
consumed = originUsed
