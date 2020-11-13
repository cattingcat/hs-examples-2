{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HsSyntax.FixAndRec where

import Prelude hiding (succ)

fix :: (a -> a) -> a
fix f = f (fix f)

factor :: Int -> Int
factor = fix (\f n -> if n > 1 then n * f (n - 1) else n)

newtype Fix f = MkFix (f (Fix f))

instance Show (f (Fix f)) => Show (Fix f) where
  show (MkFix f) = "Fix " ++ show f

data FNat a = Zero | Succ a
  deriving stock (Show)

type Nat = Fix FNat

zero :: Nat
zero = MkFix Zero

succ :: Nat -> Nat
succ = MkFix . Succ

nat2Int :: Nat -> Int
nat2Int (MkFix Zero) = 0
nat2Int (MkFix (Succ a)) = 1 + nat2Int a

--foldFix' :: (f (Fix f) -> b -> (b, Fix f)) -> b -> Fix f -> b
--foldFix' f z (MkFix a) = let ~(res, fx) = f a z in foldFix' f res fx

fold :: Functor f => (f b -> b) -> Fix f -> b
fold f (MkFix a) = f (fmap (fold f) a)

unfold :: Functor f => (b -> f b) -> b -> Fix f
unfold f b = MkFix $ fmap (unfold f) (f b)

class (Functor f, Functor g) => NatTranstorm f g where
  transform :: f a -> g a

class Comonad m where
  copure :: m a -> a
  cojoin :: m a -> m (m a)

data Stream a = a :& Stream a

instance Comonad Stream where
  copure (a :& _) = a
  cojoin s = s :& cojoin s

tst = succ (succ (succ (succ zero)))
