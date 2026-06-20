{-# LANGUAGE CPP #-}
module CppExpandSafe
  ( listed
  ) where

listed :: Int
listed = 1

#ifdef SOME_FLAG
flagged :: Int
flagged = 2
#endif

missing :: Int
missing = 3
