{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

module Lenses.Composability
  ( Lens,
  )
where

import Data.Function (on)
import Data.Functor.Const
import Data.Functor.Identity
import Data.Int
import Data.Monoid

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Setting s t a b = (a -> Identity b) -> s -> Identity t

(@.) :: Lens' b c -> Lens' a b -> Lens' a c
(@.) lbc lab f s = lab (lbc f) s
