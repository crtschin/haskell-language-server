{-# LANGUAGE CPP #-}
module CppUnexportClean
  ( foo
  , bar
#ifdef EXAMPLE_FLAG
  , flagged
#endif
  ) where

foo :: Int
foo = 1

bar :: Int
bar = 2

flagged :: Int
flagged = 3
