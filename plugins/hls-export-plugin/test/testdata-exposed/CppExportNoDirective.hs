{-# LANGUAGE CPP #-}
module CppExportNoDirective (alpha) where

#ifdef NOPE
extra :: Int
extra = 0
#endif

alpha :: Int
alpha = 1
