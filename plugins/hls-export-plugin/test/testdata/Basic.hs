{-# OPTIONS_GHC -Wunused-top-binds -XPatternSynonyms #-}
module Basic (foo, Bar, Baz (Baz1), Foo (Foo1, Foo2), Qux (..), pattern Quux1, Bare) where

foo :: Int
foo = 1

bar :: Int
bar = 2

data Bar = Bar
data Baz = Baz1 | Baz2
data Foo = Foo1 | Foo2 | Foo3
data Qux = Qux1 | Qux2
data Quux = Quux1 | Quux2
data Bare = Bare1 | Bare2
data Unlisted = Unlisted1 | Unlisted2

withWhere :: ()
withWhere = whereBound
  where whereBound = ()

data Rec = Rec { recField :: () }
