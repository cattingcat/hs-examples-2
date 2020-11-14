{-# LANGUAGE TypeOperators #-}

module Lenses.OneMoreAttempt.ProfunctorStuff where

import Data.Profunctor (Choice (..), Profunctor (..))

type a \/ b = Either a b

newtype Tagged t a = Tagged a

instance Profunctor Tagged where
  dimap :: (a -> b) -> (c -> d) -> Tagged b c -> Tagged a d
  dimap _ g (Tagged a) = Tagged (g a)

instance Choice Tagged where
  left' :: Tagged a b -> Tagged (a \/ c) (b \/ c)
  left' (Tagged b) = Tagged (Left b)

newtype Forget r a x = Forget (a -> r)

instance Profunctor (Forget r) where
  dimap :: (a -> b) -> (c -> d) -> Forget r b c -> Forget r a d
  dimap f _ (Forget h) = Forget (h . f)

instance Choice (Forget r) where
  left' :: Forget r a b -> Forget r (a \/ c) (b \/ c)
  left' (Forget f) = Forget foo
    where
      foo (Left a) = f a
      foo (Right c) = undefined -- Forget isn't Choice

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap :: (s' -> s) -> (t -> t') -> Exchange a b s t -> Exchange a b s' t'
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)

instance Choice (Exchange a b) where
  left' :: Exchange a b s t -> Exchange a b (s \/ _1) (t \/ _2)
  left' (Exchange sa bt) = Exchange foo (Left . bt)
    where
      foo (Left s) = sa s
      foo (Right _) = undefined -- Exchange isn't Choice profunctor
