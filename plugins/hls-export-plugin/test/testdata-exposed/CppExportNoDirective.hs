{-# LANGUAGE CPP #-}
module CppExportNoDirective (alpha, beta) where

#ifdef NOPE
extra :: Int
extra = 0
#endif

alpha :: Int
alpha = 1

beta :: Int
beta = 2
