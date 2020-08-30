module Design.Freer.MtlMonads () where

import Control.Monad ((>=>))


data Reader s a = Pure a | Get (s -> Reader s a)

instance Functor (Reader s) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Get g) = Get (fmap f . g)

instance Applicative (Reader s) where
  pure = Pure
  (Pure f) <*> (Pure a) = Pure (f a)
  (Pure f) <*> a = f <$> a
  f@(Get _) <*> (Pure a) = ($ a) <$> f
  (Get f) <*> (Get a) = Get (\s -> f s <*> a s)

instance Monad (Reader s) where
  Pure a >>= f = f a
  Get a  >>= f = Get (a >=> f) -- (\s -> a s >>= f)

runReader :: s -> Reader s a -> a
runReader _ (Pure a) = a
runReader a (Get fa) = runReader a (fa a)





data RWer w s a = Pure2 a
                | Get2 (s -> RWer w s a)
                | Put w (() -> RWer w s a) 