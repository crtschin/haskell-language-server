module UsesReexportFacade where

import           ReexportFacade (originUsed)

consumed :: Int
consumed = originUsed
