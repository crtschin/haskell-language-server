{-# LANGUAGE CPP #-}
module CppCtorUnexport
  ( Foo
      ( Foo1
      , Foo2
#ifdef EXAMPLE_FLAG
      , Foo3
#endif
      )
  , Bar
      ( Bar1
#ifdef EXAMPLE_FLAG
      , Bar2
#endif
      )
  ) where

data Foo = Foo1 | Foo2 | Foo3
data Bar = Bar1 | Bar2
