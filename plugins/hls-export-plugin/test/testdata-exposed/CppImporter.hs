{-# LANGUAGE CPP #-}
module CppImporter where

import qualified CppImported

shown :: Int
shown = CppImported.shown

#ifdef NOPE
hidden :: Int
hidden = CppImported.hidden
#endif
