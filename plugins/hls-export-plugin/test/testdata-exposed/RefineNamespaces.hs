module RefineNamespaces where

data a :+: b = MkSum a b

pattern Used :: Int
pattern Used = 7

usedValue :: Int
usedValue = 1

unusedValue :: Int
unusedValue = 2
