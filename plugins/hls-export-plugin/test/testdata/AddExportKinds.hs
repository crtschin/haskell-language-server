{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module AddExportKinds (placeholder) where

placeholder :: Int
placeholder = 0

(<|) :: a -> a -> a
(<|) x _ = x

type (:<>) = ()

type family (:+:)

data (:<) = Mk

pattern Pat :: a -> (a, a)
pattern Pat a = (a, a)

class Cls a where
  method :: a -> Int
