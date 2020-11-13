{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Extensions.GeneralisedNewtypeDeriving () where

import Control.Monad.State (State (..), StateT (..))

newtype Dollars = Dollars Int
  deriving newtype (Num) -- Derives Typeclass from representational type (Int in this case)

class AClass a where
  aMethod :: a -> (# Int, a #)

instance AClass Int where
  aMethod a = (# a, a #)

newtype Int' = Int' Int
  deriving newtype (AClass)

-- Doesn't work
--newtype NonMonad m s = NonMonad (StateT s m s) deriving Monad

--newtype NonMonad m s = NonMonad (StateT Int m s)
--  deriving newtype (Functor, Applicative, Monad)
--
--instance Monad (StateT s m) => Monad (NonMonad m)

-- | Associated type family
class HasRing a where
  type Ring a

--  f :: Ring a

newtype L1Norm a = L1Norm a
  deriving newtype (HasRing)

-- | Associated data family
class Ex a where
  data D a

--  f :: D a

instance Ex Int where
  data D Int = DInt Bool

-- doesn't work with data families
--newtype Age = MkAge Int
--  deriving newtype Ex

foo :: D Int
foo = DInt True
