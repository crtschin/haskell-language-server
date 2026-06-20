{-# LANGUAGE CPP #-}
module CppUnexportWedged
  ( foo
#ifdef EXAMPLE_FLAG
  , flagged
#endif
  , bar
  ) where

foo :: Int
foo = 1

bar :: Int
bar = 2

flagged :: Int
flagged = 3
