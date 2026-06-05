module ExportAll where

value :: Int
value = 1

data Rec = Rec { field :: Int }

newtype NT = NT Int

class Cls a where
  method :: a -> Int

type Syn = Int
