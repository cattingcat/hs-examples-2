{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Extensions.FlexibleContexts () where

import Data.Data (Proxy (..))

class Show a => MyClass a where
  foo :: a -> String

class Functor (m k) => FiniteMap m k where
  tst :: (a -> b) -> m k a -> m k b

class A cls a where
  meth :: cls a => Proxy cls -> a -> a

tstA :: (Show a, A Show a) => a -> a
tstA a = meth (Proxy :: Proxy Show) a
