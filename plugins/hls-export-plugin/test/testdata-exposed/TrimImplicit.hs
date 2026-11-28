module TrimImplicit {-# DEPRECATED "old" #-} where

data T = MkA | MkB

data U = MkU

data a :+: b = MkSum a b

pattern Used :: Int
pattern Used = 7

usedValue :: Int
usedValue = 1

unusedValue :: Int
unusedValue = 2

usedOnlyInternally :: Int
usedOnlyInternally = helper + 2

helper :: Int
helper = 3
