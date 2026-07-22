module PatternClash (T, pattern T) where

data T = MkT

pattern T :: T
pattern T = MkT
