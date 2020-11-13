{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Extensions.StandaloneDeriving () where

import Control.Monad.State

data T0 f a = MkT0 a deriving (Eq)

data T1 f a = MkT1 (f a) deriving (Eq)

data T2 f a = MkT2 (f (f a)) -- deriving( Eq ) Impossible to derive by default

deriving instance Eq (f (f a)) => Eq (T2 f a)

data T a where
  T1 :: T Int
  T2 :: T Bool

--    deriving (Show) - impossible to derive GADTs
deriving stock instance Show (T a)

-- GHC always treats the last parameter of the instance
--  (Foo in this example) as the type whose instance is being derived.

newtype Foo a = MkFoo (State Int a)
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadState Int Foo
