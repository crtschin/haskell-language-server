module ExportLayout
  ( foo -- the foo
  , bar -- the bar
  -- * Types
  , Foo
      ( Foo1 -- first
      , Foo2 -- second
      , Foo3 -- third
      )
  ) where

foo :: Int
foo = 1

bar :: Int
bar = 2

data Foo = Foo1 | Foo2 | Foo3

quux :: Int
quux = 3
