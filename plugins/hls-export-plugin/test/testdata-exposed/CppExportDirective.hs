{-# LANGUAGE CPP #-}
module CppExportDirective
  ( alpha
#ifdef NOPE
  , beta
#endif
  ) where

alpha :: Int
alpha = 1

#ifdef NOPE
beta :: Int
beta = 2
#endif
