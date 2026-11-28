{-# LANGUAGE PatternSynonyms #-}
module ExportAllImplicit {-# DEPRECATED "old" #-} where

value :: Int
value = 1

(<|) :: a -> a -> a
(<|) x _ = x

data Rec = Rec { field :: Int }

class Cls a where
  method :: a -> Int

type Syn = Int

pattern Pat :: a -> a
pattern Pat a = a

(left, right) = (2 :: Int, 3 :: Int)

foreign import ccall "sin" c_sin :: Double -> Double
