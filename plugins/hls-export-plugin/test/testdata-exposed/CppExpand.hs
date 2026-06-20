{-# LANGUAGE CPP #-}
module CppExpand
  ( listed
#ifdef SOME_FLAG
  , flagged
#endif
  ) where

listed :: Int
listed = 1

flagged :: Int
flagged = 2

missing :: Int
missing = 3
