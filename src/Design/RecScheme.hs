{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Design.RecScheme () where
import Prelude ()
import Relude

--data List a = Nil | Cons a (List a)
--  deriving stock (Show)

data ListF a x = NilF | ConsF a x
  deriving stock (Show, Functor)

type Algebra f c = f c -> c

sum :: Algebra (ListF Int) Int
sum NilF = 0
sum (ConsF a c) = a + c

-- Algebra for given Functor form Category
--  with initial object - initial algebra

-- carrier for initial algebra - Fix
newtype Fix f = In { out :: f (Fix f) }

-- So we have arrows from initial object to others
--  Fix f -> a
-- catamorphism (from catastrophe)

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg =  alg . fmap (cata alg). out


type List a = Fix (ListF a)
foldr :: (a -> c -> c) -> c -> List a -> c
foldr f z l = cata foo l
  where
    foo NilF = z
    foo (ConsF a c) = f a c


-- Endofunctor category
-- Objects - Functors
-- Arrows - nat transformation

-- Original functor: Functor f => (a -> b) -> (f a -> f b)
-- Higher functor: Functor f, Functor g => (g ~> f) -> (hf f ~> hf g)

type f :~> g = forall a . f a -> g a
infixr 0 :~>

class HFunctor hf where
  hfmap :: (f :~> g) -> (hf f :~> hf g)
  ffmap :: Functor f => (a -> b) -> hf f a -> hf f b

type HAlgebra hf f = hf f :~> f

newtype FixH hf a = InH { outH :: hf (FixH hf) a }

hcata :: (HFunctor hf) => HAlgebra hf f -> FixH hf :~> f
hcata halg = halg . hfmap (hcata halg) . outH





class Monad' m where
  eta :: Identity :~> m
  mu :: Compose m m :~> m

data (f :+: g) a = InL (f a) | InR (g a)
type FunctorList f g = Identity :+: Compose f g
-- MonadM is the same as FunctorList
data MonadM f g a = DoneM a
                  | MoreM (f (g a))

type FreeMonad f = FixH (MonadM f)

instance Functor (FreeMonad f)

instance Applicative (FreeMonad f) where 
  pure = InH . DoneM

instance Functor f => Monad (FreeMonad f) where
  (InH (DoneM a))    >>= k = k a
  (InH (MoreM ffra)) >>= k = InH (MoreM (fmap (>>= k) ffra))