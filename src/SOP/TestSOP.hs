{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module SOP.TestSOP () where

-- see generic-sop library

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.Generics as GHC

class (f (g x)) => Compose f g x

instance (f (g x)) => Compose f g x

type All ::
  (k -> Constraint) -> [k] -> Constraint
type family
  All c ts
  where
  All _ '[] = ()
  All c (t ': ts) = (c t, All c ts)

-- | N-products
data NP :: (k -> Type) -> [k] -> Type where
  Nil :: NP f '[]
  (:*) :: f x -> NP f xs -> NP f (x ': xs)

-- | Products o products
type POP :: (k -> Type) -> [[k]] -> Type
newtype POP f xss = POP {unPOP :: NP (NP f) xss}

-- | N-sums
data NS :: (k -> Type) -> [k] -> Type where
  Z :: f x -> NS f (x ': xs)
  S :: NS f xs -> NS f (x ': xs)

-- | Sum of products
type SOP :: (k -> Type) -> [[k]] -> Type
newtype SOP f xss = SOP (NS (NP f) xss)
  deriving newtype (Show)

-- NP (K Int) '[Char, Bool]
newtype K (a :: Type) (b :: k) = K a
  deriving stock (Show)

-- NP I '[Char, Bool]
newtype I (a :: Type) = I a
  deriving stock (Show)

indexNS :: NS f xs -> Int
indexNS (Z _) = 0
indexNS (S r) = 1 + indexNS r

instance All (Show `Compose` f) xs => Show (NP f xs) where
  show Nil = "Nil"
  show (fx :* np) = show fx <> " " <> show np

instance All (Show `Compose` f) xs => Show (NS f xs) where
  show (Z fa) = "Z " <> show fa
  show (S as) = "S " <> show as

type family ToSingleCode (a :: Type -> Type) :: Type

type instance ToSingleCode (K1 _i a) = a

type family ToProductCode (a :: Type -> Type) (xs :: [Type]) :: [Type]

type instance ToProductCode (a :*: b) xs = ToProductCode a (ToProductCode b xs)

type instance ToProductCode U1 xs = xs

type instance ToProductCode (M1 S _c a) xs = ToSingleCode a ': xs

type family ToSumCode (a :: Type -> Type) (xs :: [[Type]]) :: [[Type]]

type instance ToSumCode (a :+: b) xs = ToSumCode a (ToSumCode b xs)

type instance ToSumCode V1 xs = xs

type instance ToSumCode (M1 D _c a) xs = ToSumCode a xs

type instance ToSumCode (M1 C _c a) xs = ToProductCode a '[] ': xs

class GSingleFrom (a :: Type -> Type) where
  gSingleFrom :: a x -> ToSingleCode a

instance GSingleFrom (K1 i a) where
  gSingleFrom (K1 a) = a

class GProductFrom (a :: Type -> Type) where
  gProductFrom :: a x -> NP I xs -> NP I (ToProductCode a xs)

instance (GProductFrom a, GProductFrom b) => GProductFrom (a :*: b) where
  gProductFrom (a :*: b) xs = gProductFrom a (gProductFrom b xs)

instance GProductFrom U1 where
  gProductFrom U1 xs = xs

instance GSingleFrom a => GProductFrom (M1 S c a) where
  gProductFrom (M1 a) xs = I (gSingleFrom a) :* xs

class GSumFrom (a :: Type -> Type) where
  gSumFrom :: a x -> Proxy xss -> SOP I (ToSumCode a xss)
  gSumSkip :: Proxy a -> SOP I xss -> SOP I (ToSumCode a xss)

instance GSumFrom V1 where
  gSumFrom x = case x of
  gSumSkip _ xss = xss

instance (GSumFrom a, GSumFrom b) => GSumFrom (a :+: b) where
  gSumFrom (L1 a) xss = gSumFrom a (toSumCodeProxy xss)
    where
      toSumCodeProxy :: Proxy xss -> Proxy (ToSumCode b xss)
      toSumCodeProxy _ = Proxy
  gSumFrom (R1 b) xss = gSumSkip (Proxy @a) (gSumFrom b xss)

  gSumSkip _ xss = gSumSkip (Proxy @a) (gSumSkip (Proxy :: Proxy b) xss)

instance (GSumFrom a) => GSumFrom (M1 D c a) where
  gSumFrom (M1 a) xss = gSumFrom a xss
  gSumSkip _ xss = gSumSkip (Proxy @a) xss

instance (GProductFrom a) => GSumFrom (M1 C c a) where
  gSumFrom (M1 a) _ = SOP (Z (gProductFrom a Nil))
  gSumSkip _ (SOP xss) = SOP (S xss)

type GCode (a :: Type) = ToSumCode (GHC.Rep a) '[]

type GFrom a = GSumFrom (GHC.Rep a)

gfrom :: (GFrom a, GHC.Generic a) => a -> SOP I (GCode a)
gfrom x = gSumFrom (GHC.from x) (Proxy :: Proxy '[])

data Tst = Tst {a :: Int, s :: String}
  deriving stock (Generic)

tst = gfrom (Tst 123 "sdf")
