{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Extensions.ConstraintKinds () where

import GHC.Base
import GHC.Exts
import Foreign.C (CInt(..))

-- | Constraint in kinds
type family IsTypeLit a where
  IsTypeLit Nat    = 'True
  IsTypeLit Symbol = 'True
  IsTypeLit a      = 'False

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