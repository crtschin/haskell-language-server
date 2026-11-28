module ReexportOrigin (originUsed, originUnused, qualUsed, fieldUsed) where

originUsed :: Int
originUsed = 1

originUnused :: Int
originUnused = 2

qualUsed :: Int
qualUsed = 3

data R = MkR { fieldUsed :: Int }
