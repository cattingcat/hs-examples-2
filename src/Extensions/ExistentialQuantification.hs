{-# LANGUAGE ExistentialQuantification #-}

module Extensions.ExistentialQuantification () where

data Foo = forall a. MkFoo a (a -> Bool) | Nil

data Baz
  = forall a. Eq a => Baz1 a a
  | forall b. Show b => Baz2 b (b -> b)

bazFoo :: Baz -> String
bazFoo (Baz1 a b) = show $ a == b
bazFoo (Baz2 a f) = show $ f a
