{-# OPTIONS_GHC -Wno-deprecations #-}
module UsesDeprecatedHeader where

import           DeprecatedHeader (usedHere)

consumed :: Int
consumed = usedHere
