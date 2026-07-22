module UsesRefineNamespaces where

import           RefineNamespaces

consumed :: Int :+: Bool
consumed = MkSum 1 True

consumedValue :: Int
consumedValue = usedValue + Used
