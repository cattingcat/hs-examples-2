{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Design.ChurchFreeMonad where

import Data.Kind

type Free :: (Type -> Type) -> Type -> Type
data Free f a = Free (f (Free f a)) | Pure a

type F :: (Type -> Type) -> Type -> Type
newtype F f a = F {unF :: forall r. (a -> r) -> (f r -> r) -> r}

instance Functor (F f) where
  fmap f (F g) = F (\pr fr -> g (pr . f) fr)

instance Functor f => Applicative (F f) where
  pure a = F (\pr fr -> pr a)

  (<*>) :: forall a b. F f (a -> b) -> F f a -> F f b
  --  F f <*> a'@(F a) = F (\pr fr -> f (\fab -> unF (fmap fab a') pr fr ) fr )
  F f <*> F g = F (\pr fr -> f (\fab -> g (pr . fab) fr) fr)

instance Functor f => Monad (F f) where
  F a >>= f = F (\pr fr -> a (\q -> unF (f q) pr fr) fr)

iter :: (f a -> a) -> F f a -> a
iter f (F frf) = frf id f

iterM :: Monad m => (f (m a) -> m a) -> F f a -> m a
iterM f (F frf) = frf pure f

liftF :: Functor f => f a -> F f a
liftF fa = F (\pr fr -> fr $ fmap pr fa)

data MyF next where
  A :: (Int, Int) -> (Int -> next) -> MyF next

instance Functor MyF where
  fmap f (A lims nxt) = A lims (f . nxt)

type MyFree a = F MyF a

interpretMyFree :: MyFree a -> IO a
interpretMyFree (F f) = f fa fb
  where
    fa :: a -> IO a
    fa a = pure a

    fb :: MyF (IO a) -> IO a
    fb (A (from, to) nxt) = do
      let r = 4
      putStrLn $ "random: " ++ show r
      nxt r

testApp :: MyFree Int
testApp = do
  r1 <- liftF $ A (1, 5) id
  r2 <- liftF $ A (1, 5) id
  pure (r1 + r2)

runTest = interpretMyFree testApp

f :: (Bool -> Bool) -> Char
f = undefined

g :: ((forall a. a -> a) -> Char) -> Int
g ff = undefined (ff id)

h :: Int
h = g f
