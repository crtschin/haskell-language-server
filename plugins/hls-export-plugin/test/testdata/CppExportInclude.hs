{-# LANGUAGE CPP #-}
#include "CppExportPrelude.h"
module CppExportInclude
  ( foo
#include "CppExportInclude.h"
  ) where

foo :: Int
foo = 1

included :: Int
included = 2

alsoIncluded :: Int
alsoIncluded = 3

thirdIncluded :: Int
thirdIncluded = 4

extra :: Int
extra = 5
