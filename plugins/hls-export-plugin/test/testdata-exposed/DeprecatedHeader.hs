module DeprecatedHeader {-# DEPRECATED "old" #-} where

usedHere :: Int
usedHere = 1

notUsed :: Int
notUsed = 2
