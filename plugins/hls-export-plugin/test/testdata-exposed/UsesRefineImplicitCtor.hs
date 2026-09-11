module UsesRefineImplicitCtor where

import           RefineImplicitCtor (T (MkA), U)

consumed :: T
consumed = MkA

described :: U -> Int
described _ = 0
