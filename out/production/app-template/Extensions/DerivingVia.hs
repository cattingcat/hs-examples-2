{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Extensions.DerivingVia () where

import Numeric (showHex)
import Data.Monoid (Product(..))
import Control.Applicative (liftA2)



newtype Dollars = Dollars Int
  deriving (Semigroup, Monoid) via (Product Int)




newtype Hex a = Hex a

instance (Integral a, Show a) => Show (Hex a) where
  show (Hex a) = "0x" ++ showHex a ""

newtype Unicode = U Int
  deriving Show via (Hex Int)
  deriving Num via Int





newtype App f a = MkApp (f a)
  deriving newtype (Functor, Applicative)

instance (Monoid a, Applicative f) => Semigroup (App f a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative f) => Monoid (App f a) where
  mempty = pure mempty

data Pair a = MkPair a a
  deriving stock Functor
  deriving (Semigroup, Monoid) via (App Pair a)

instance Applicative Pair where
  pure a = MkPair a a
  MkPair f g <*> MkPair a b = MkPair (f a) (g b)




newtype Kleisli m a b = Kleisli (a -> m b)
  deriving (Semigroup, Monoid) via (a -> App m b) -- coersible

-- Syntax for standalone
--deriving via (a -> App m b) instance Monoid (Kleisli m a b)