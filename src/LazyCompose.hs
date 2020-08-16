{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module LazyCompose where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Kind (Type)



data MyKleisli m a b where
  One :: Kleisli m a b -> MyKleisli m a b
  Comp :: Kleisli m a b -> MyKleisli m b c -> MyKleisli m a c

comp' :: MyKleisli m a b -> MyKleisli m b c -> MyKleisli m a c
comp' (One kl) mkl = Comp kl mkl
comp' (Comp kl co) mkl = Comp kl (comp' co mkl)

unKleisli :: Monad m => MyKleisli m a b -> Kleisli m a b
unKleisli (One f) = f
unKleisli (Comp f co) = Kleisli (runKleisli f >=> runKleisli (unKleisli co))


instance Monad m => Category (MyKleisli m) where
  id = One id
  (.)  b (One ar) = Comp ar b
  (.)  b (Comp ar a) = Comp ar (b . a)

instance Applicative m => Functor (MyKleisli m a) where
  fmap f a =  comp' a (One (Kleisli $ pure . f))

instance Monad m => Applicative (MyKleisli m a) where
  pure a = One (pure a)
  f <*> a = One $ unKleisli f <*> unKleisli a

--instance Monad m => Monad (MyKleisli m a) where
--  (>>=) :: MyKleisli m a b -> (b -> MyKleisli m a c) -> MyKleisli m a c  
  
--  One kl     >>= f = One $ kl >>= (unKleisli . f)
--  Comp kl co >>= f = Comp kl 
