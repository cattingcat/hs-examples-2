{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Extensions.StandaloneKindSignatures () where

import Data.Data (Proxy (..))
import GHC.Base

type App :: (k -> Type) -> k -> Type
newtype App f a = MkApp (f a)

type T :: (k1 -> Type) -> k -> k1 -> Type
newtype T a b c = MkT (a c)

type F0 :: k -> Type
type family F0 a where
  F0 App = Int
  F0 String = Int

type T1 :: Type -> Type
data family T1

data instance T1 Int = String

data instance T1 String = Int

type T2 :: (Type -> Type) -> (Type -> Type)
data family T2

data instance T2 [] a = Maybe a

data instance T2 Maybe a = Either Int a

type T3 :: k -> Type
data family T3

data instance T3 (a :: MyNat) where
  MkN1 :: T3 ( 'Su 'Ze)
  MkN2 :: T3 ( 'Su ( 'Su 'Ze))

type TS a (b :: k) = (k, a, Proxy b)

foo :: TS Int 'Ze
foo = (Su Ze, 5, Proxy)

type G :: k -> Type
data G a where
  GInt :: G Int
  GMaybe :: G Maybe
  GZe :: G 'Ze

-- | Rewrite Vect/HList with standalone king sigs
type MyNat :: Type
data MyNat = Ze | Su MyNat

class KnowsMyNat (n :: MyNat) where natVal :: Proxy n -> Int

instance KnowsMyNat 'Ze where natVal _ = 0

instance forall n. (KnowsMyNat n) => KnowsMyNat ( 'Su n) where natVal _ = 1 + (natVal $ Proxy @n)

type Vect :: Type -> MyNat -> Type
data Vect a sz where
  Nil :: Vect a 'Ze
  Cons :: a -> Vect a n -> Vect a ( 'Su n)

(*-) = Cons

infixr 7 *-

len :: forall a n. KnowsMyNat n => Vect a n -> Int
len _ = natVal (Proxy @n)

type HList :: [Type] -> Type
data HList ts where
  HNil :: HList '[]
  HCons :: a -> HList ts -> HList (a ': ts)

(*~) = HCons

infixr 7 *~

type Times :: k -> MyNat -> [k]
type family Times a n where
  Times _ 'Ze = '[]
  Times a ( 'Su n) = a ': Times a n

type Size :: [k] -> MyNat
type family Size as where
  Size '[] = 'Ze
  Size (_ : ts) = 'Su (Size ts)

vect2hlist :: Vect a n -> HList (Times a n)
vect2hlist Nil = HNil
vect2hlist (Cons a v) = HCons a (vect2hlist v)

hlist2vect :: (Times a (Size ts) ~ ts) => HList ts -> Vect a (Size ts)
hlist2vect HNil = Nil
hlist2vect (HCons a hs) = Cons a (hlist2vect hs)

instance Show (HList '[]) where show _ = "HNil"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where show (HCons a hs) = show a ++ " *~ " ++ show hs

instance Show a => Show (Vect a n) where
  show Nil = "Nil"
  show (Cons a t) = show a ++ " *- " ++ show t

tst1 = "qwe" *~ vect2hlist (1 *- 2 *- 3 *- Nil)

tst2 = hlist2vect (1 *~ 2 *~ 3 *~ HNil)
