{-# LANGUAGE CPP #-}
module CppHeaderOnly
#ifdef NOPE
  ( alpha )
#endif
  where

alpha :: Int
alpha = 1
