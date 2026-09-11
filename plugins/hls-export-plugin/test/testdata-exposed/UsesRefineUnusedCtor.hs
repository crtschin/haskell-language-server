module UsesRefineUnusedCtor where

import           RefineUnusedCtor (T (MkA), U)

consumed :: T
consumed = MkA

described :: U -> Int
described _ = 0
