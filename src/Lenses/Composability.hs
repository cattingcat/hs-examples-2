{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lenses.Composability (
  Lens
) where

import Data.Functor.Identity
import Data.Functor.Const
import Data.Int
import Data.Function (on)
import Data.Monoid

type Lens s t a b = ∀ f . Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

type Traversal s t a b = ∀ f . Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

type Getting r s a   = (a -> Const r a)  -> s -> Const r s
type Setting s t a b = (a -> Identity b) -> s -> Identity t


(@.) :: Lens' b c -> Lens' a b -> Lens' a c
(@.) lbc lab f s = lab (lbc f) s