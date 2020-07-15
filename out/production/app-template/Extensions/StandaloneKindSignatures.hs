{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Extensions.StandaloneKindSignatures () where

import GHC.Base


type    App :: (k -> Type) -> k -> Type
newtype App f a = MkApp (f a)

type    T :: (k1 -> Type) -> k -> k1 -> Type
newtype T a b c = MkT (a c)








type MyNat :: Type
data MyNat = Ze | Su MyNat

type Vect :: Type -> MyNat -> Type
data Vect a sz where
  Nil :: Vect a 'Ze
  Cons :: a -> Vect a n -> Vect a ('Su n)

(*-) = Cons
infixr 7 *-

type HList :: [Type] -> Type
data HList ts where
  HNil :: HList '[]
  HCons :: a -> HList ts -> HList (a ': ts)

(*~) = HCons
infixr 7 *~


type        Times :: k -> MyNat -> [k]
type family Times a n where
  Times _ 'Ze = '[]
  Times a ('Su n) = a ': Times a n

type        Size :: [k] -> MyNat
type family Size as  where
  Size '[] = 'Ze
  Size (_:ts) = 'Su (Size ts)

vect2hlist :: Vect a n -> HList (Times a n)
vect2hlist Nil = HNil
vect2hlist (Cons a v) = HCons a (vect2hlist v)

hlist2vect :: (Times a (Size ts) ~ ts) => HList ts -> Vect a (Size ts)
hlist2vect HNil = Nil
hlist2vect (HCons a hs) = Cons a (hlist2vect hs)

instance Show (HList '[]) where show _ = "HNil"
instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where show (HCons a hs) = show a ++ " *~ " ++ show hs





tst1 = "qwe" *~ vect2hlist (1 *- 2 *- 3 *- Nil)