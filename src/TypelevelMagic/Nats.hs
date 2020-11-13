{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TypelevelMagic.Nats () where

data family Sing (n :: k)

class SigI (a :: k) where
  sing :: Sing a

data SomeSing k where
  SomeSing :: forall k (n :: k). SigI n => Sing n -> SomeSing k

data Nat = Z | S Nat

instance SigI 'Z where sing = SingZ

instance SigI (n :: Nat) => SigI ( 'S n) where sing = SingS sing

data instance Sing (_ :: Nat) where
  SingZ :: Sing 'Z
  SingS :: Sing a -> Sing ( 'S a)

toSing :: Nat -> SomeSing Nat
toSing Z = SomeSing SingZ
toSing (S n) = case toSing n of
  SomeSing (a :: Sing nk) -> SomeSing (SingS a)

intToNat :: Int -> Nat
intToNat 0 = Z
intToNat n = S (intToNat (n - 1))

type family (<=) (a :: Nat) (b :: Nat) :: Bool where
  'Z <= _ = 'True
  'S a <= 'S b = a <= b
  _ <= _ = 'False

withMatrix :: (Int, Int) -> (forall (m :: Nat) (n :: Nat). (SigI m, SigI n) => Matrix m n -> r) -> r
withMatrix (m, n) f = case (toSing (intToNat m), toSing (intToNat n)) of
  (SomeSing (_ :: Sing ms), SomeSing (_ :: Sing ns)) ->
    f (MkMatrix :: Matrix ms ns)

data Matrix (m :: Nat) (n :: Nat) = MkMatrix

foo :: forall (a :: Nat) (b :: Nat). (a <= b ~ 'True) => Matrix a b -> Int
foo _ = 42

data (:<=:) (a :: k) (b :: k) where
  LEQ :: (l <= g ~ 'True) => l :<=: g

data Decision a = Proved a | Disproved

class LEQDecide k where
  check :: forall (m :: k) (n :: k). Sing m -> Sing n -> Decision (m :<=: n)

instance LEQDecide Nat where
  check SingZ SingZ = Proved LEQ
  check SingZ (SingS _) = Proved LEQ
  check (SingS _) SingZ = Disproved
  check (SingS a) (SingS b) = case check a b of
    Proved LEQ -> Proved LEQ
    Disproved -> Disproved

test :: Int
test = withMatrix (5, 10) processMatr
  where
    processMatr :: forall (m :: Nat) (n :: Nat). (SigI m, SigI n) => Matrix m n -> Int
    processMatr mtr = case check (sing :: Sing m) (sing :: Sing n) of
      Proved LEQ -> foo mtr
      Disproved -> 0
