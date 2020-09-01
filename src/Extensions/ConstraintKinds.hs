{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Extensions.ConstraintKinds () where

import GHC.Base
import GHC.Exts
import Data.Proxy
import GHC.TypeLits

-- | Constraint in kinds
type family IsTypeLit a where
  IsTypeLit Nat    = 'True
  IsTypeLit Symbol = 'True
  IsTypeLit _      = 'False

data T :: forall a. (IsTypeLit a ~ 'True) => a -> Type where
  MkNat    :: T 42
  MkSymbol :: T "Don't panic!"

--data StarT (a :: * -> *) where
--  St :: StarT Maybe


-- Levity polymorphic
-- GENERIC input params can't be polymorphic because of aligning and so on
($$) :: forall r (a :: Type) (b :: TYPE r).
       (a -> b) -> a -> b
f $$ x = f x

--bad :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (a :: TYPE r1) (b :: TYPE r2).
--       (a -> b) -> a -> b
--bad f x = f x

fun :: Int -> Int#
fun (I# i) = i

g :: Int# -> Int#
g a = a

tst :: Int# -> Int#
tst _ = fun $$ 55

--tst2 _ = g $$ 55



type Typ :: Type -> Type -> Constraint
type family Typ a b
type instance Typ Int  b = Show b
type instance Typ Bool b = Num b

func :: Typ a b => a -> b -> b
func = undefined


type family (:?) (a :: k) (ts :: [k]) :: Constraint where
  (:?) _ '[] = TypeError ('Text "a not in ts")
  (:?) k (k ': _) = ()
  (:?) k (_ ': ks) = (:?) k ks

foo :: (i :? ts) => i -> Proxy ts -> Bool
foo _ _ = True

tst1 = foo (55 :: Int) (Proxy :: Proxy [Int, String])
--tst2 = foo (55 :: Integer) (Proxy :: Proxy [Int, String]) -- type error