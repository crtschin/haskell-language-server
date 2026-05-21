module MakeExplicitUsed where

usedByOther :: Int
usedByOther = 1

usedOnlyInternally :: Int
usedOnlyInternally = helper + 2

helper :: Int
helper = 3

unusedEntirely :: Int
unusedEntirely = 4

data T = MkT Int
