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

module Lenses.AppLens () where

import Data.Function (on)
import Data.Functor.Const
import Data.Functor.Identity
import Data.Int
import Data.Monoid

-- | AppLens actually named traversal
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Setting s t a b = (a -> Identity b) -> s -> Identity t

all' :: Eq a => a -> Traversal' [a] a
all' v f = traverse upd
  where
    upd a = if v == a then f a else pure a

view :: Getting a s a -> s -> a
view l s = getConst (l Const s)

over :: Setting s t a b -> (a -> b) -> s -> t
over l f s = runIdentity (l (Identity . f) s)

set :: Setting s t a b -> b -> s -> t
set l a s = runIdentity (l (Identity . const a) s)

-- set (all' 0) (-8) [100, 600, 0, 200, 0]

-- We need mempty for empty res of view
instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0

--tst2 :: (Monoid a, Num a, Eq a) => a
--tst2 = view (all' 0) [0, 1, 0, 1]

toListOf :: Getting [a] s a -> s -> [a]
toListOf l s = getConst $ l (\x -> Const [x]) s

--tst3 = toListOf (all' 0) [0, 1, 0, 1]

has :: Getting Any s a -> s -> Bool
has l s = getAny . getConst $ l (\x -> Const (Any True)) s

preview :: Getting (First a) s a -> s -> Maybe a
preview l s = getFirst . getConst $ l (Const . First . Just) s

tst4 = preview (all' 0) [1, 2, 3, 0]

-- | toListOf :: Getting [a] s a -> s -> [a]
--   all' is expensive because of ([a] ++ [a] ++ ...)
toListOf' :: Getting (Endo [a]) s a -> s -> [a]
toListOf' l s = appEndo (getConst (l func s)) []
  where
    func x = Const (Endo (x :))

tst5 = toListOf' (all' 5) [1, 2, 3, 4, 5, 5, 6, 5, 7, 8, 9]

class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b
  --  default each :: (Applicative f, Traversable g, s ~ g a, t ~ g b) => (a -> f b) -> s -> f t
  default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b
  each = traverse

-- | Naive attempt
-- instance Each [a] [b] a b where
--  each _ [] = pure []
--  each f (x:xs) = (:) <$> f x <*> each f xs

-- | Any traversable can have Each instance
-- instance Traversable t => Each (t a) (t b) a b where
--  each = traverse

-- | Default implementation and standalone deriving
deriving anyclass instance Traversable t => Each (t a) (t b) a b

tst6 = set each 8 [1, 2, 3, 4, 5]
