module ReexportMiddle (originUsed, originUnused, localUnused) where

import           ReexportOrigin (originUnused, originUsed)

localUnused :: Int
localUnused = 3
