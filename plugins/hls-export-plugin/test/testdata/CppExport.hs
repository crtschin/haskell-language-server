{-# LANGUAGE CPP #-}
#include "CppExportPrelude.h"
module CppExport
  ( foo
  , Bar
#include "CppExportInclude.h"
#ifdef EXAMPLE_FLAG
  , flagged
#endif
  ) where

foo :: Int
foo = 1

data Bar = Bar1 | Bar2

included, alsoIncluded, thirdIncluded, flagged, extra :: Int
included = 2
alsoIncluded = 3
thirdIncluded = 4
flagged = 5
extra = 6
