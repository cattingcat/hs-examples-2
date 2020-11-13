module Design.Freer.RWer () where

import Control.Monad ((>=>))

data RWer s w a
  = Pure a
  | Get (s -> RWer s w a)
  | Put w (() -> RWer s w a)

instance Functor (RWer s w) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Get g) = Get (fmap f . g)
  fmap f (Put w g) = Put w (fmap f . g)

instance Applicative (RWer s w) where
  pure = Pure
  Pure f <*> a = f <$> a
  f@(Get _) <*> (Pure a) = ($ a) <$> f
  (Get f) <*> (Get a) = Get (\s -> f s <*> a s)
  (Get f) <*> (Put w a) = Put w (const $ Get (\s -> f s <*> a ()))
  f@(Put _ _) <*> (Pure a) = ($ a) <$> f
  (Put w f) <*> a = Put w (const $ f () <*> a)

instance Monad (RWer w s) where
  Pure a >>= f = f a
  Get a >>= f = Get (a >=> f) -- (\s -> a s >>= f)
  Put w a >>= f = Put w (a >=> f) -- (\s -> a s >>= f)

runRWer :: Monoid w => s -> RWer s w a -> (w, a)
runRWer s rw = loop s rw
  where
    loop s (Pure a) = (mempty, a)
    loop s (Get f) = loop s (f s)
    loop s (Put w f) = let (w', a) = loop s (f ()) in (w <> w', a)
