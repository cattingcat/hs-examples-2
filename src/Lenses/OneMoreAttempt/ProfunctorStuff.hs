module Lenses.OneMoreAttempt.ProfunctorStuff () where

import Data.Profunctor (Choice (..), Profunctor (..))

newtype Tagged t a = Tagged a

instance Profunctor Tagged where
  dimap :: (a -> b) -> (c -> d) -> Tagged b c -> Tagged a d
  dimap _ g (Tagged a) = Tagged (g a)

instance Choice Tagged where
  left' :: Tagged a b -> Tagged (Either a c) (Either b c)
  left' (Tagged b) = Tagged (Left b)

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap :: (s' -> s) -> (t -> t') -> Exchange a b s t -> Exchange a b s' t'
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)

instance Choice (Exchange a b) where
  left' :: Exchange a b s t -> Exchange a b (Either s _1) (Either t _2)
  left' (Exchange sa bt) = Exchange foo (Left . bt)
    where
      foo (Left s) = sa s
      foo (Right _) = undefined -- Exchange isn't Choice profunctor
