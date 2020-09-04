{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Design.Cps (
  Cont(..),
  ContT(..)
) where

newtype Cont r a = Cont { unCont :: (a -> r) -> r }

instance Functor (Cont r) where 
  fmap f (Cont g) = Cont (\k -> g (k . f))

instance Applicative (Cont r) where 
  pure a = Cont (\f -> f a)
  (<*>) (Cont f) (Cont a) = Cont (\k -> f (\f' -> a (k . f')))
  
instance Monad (Cont r) where 
  (>>=) (Cont a) f = Cont (\k -> a (\a' -> unCont (f a') k ))
  
  
newtype ContT r m a = ContT { unContT :: (a -> m r) -> m r }

instance Functor (ContT r m) where 
  fmap f (ContT g) = ContT (\k -> g (k . f))
  
instance Applicative (ContT r m) where 
  pure a = ContT (\f -> f a)  
  (<*>) (ContT f) (ContT a) = ContT (\k -> f (\f' -> a (k . f')))
  
instance Monad (ContT r m) where 
  (>>=) (ContT a) f = ContT (\k -> a (\a' -> unContT (f a') k))
  





data Dict c where 
  Dict :: c => Dict c
  NoInst :: Dict c
  
foo :: Dict (Show a) -> a -> String
foo Dict   a = show a
foo NoInst _ = "we"