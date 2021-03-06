{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Extensions.DataKinds
  ( MyNat (..),
    Vect (..),
    HList (..),
    type Times,
    type Size,
    type AllHas,
    vect2hlist,
    hlist2vect,
  )
where

import GHC.Base

data MyNat = Ze | Su MyNat

data Vect (a :: Type) (sz :: MyNat) where
  Nil :: Vect a 'Ze
  Cons :: a -> Vect a n -> Vect a ( 'Su n)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  HCons :: a -> HList ts -> HList (a ': ts)

type family Times (a :: Type) (n :: MyNat) :: [Type] where
  Times _ 'Ze = '[]
  Times a ( 'Su n) = a ': Times a n

type family Size (as :: [Type]) :: MyNat where
  Size '[] = 'Ze
  Size (_ : ts) = 'Su (Size ts)

type family AllHas (c :: Type -> Constraint) (l :: [Type]) :: Constraint where
  AllHas _ '[] = ()
  AllHas f (t ': ts) = (f t, AllHas f ts)

vect2hlist :: Vect a n -> HList (Times a n)
vect2hlist Nil = HNil
vect2hlist (Cons a v) = HCons a (vect2hlist v)

hlist2vect :: (Times a (Size ts) ~ ts) => HList ts -> Vect a (Size ts)
hlist2vect HNil = Nil
hlist2vect (HCons a hs) = Cons a (hlist2vect hs)

-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#promoting-existential-data-constructors
