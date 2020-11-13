{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Extensions.DeriveGeneric () where

import Control.Monad.Identity (Identity (..))
import Data.Function ((&))
import Data.Functor.Const (Const (..))
import GHC.Base (Symbol, Type)
import GHC.Generics
import GHC.OverloadedLabels (IsLabel (..))

type family HasFieldP (field :: Symbol) (t :: k -> Type) :: Maybe Type where
  HasFieldP field (M1 D _ (M1 C _ product)) = HasFieldP field product
  HasFieldP field (_ :*: M1 S ( 'MetaSel ( 'Just field) _ _ _) (K1 R t)) = 'Just t
  HasFieldP field (r :*: _) = HasFieldP field r
  HasFieldP field (M1 S ( 'MetaSel ( 'Just field) _ _ _) (K1 R t)) = 'Just t
  HasFieldP _ _ = 'Nothing

class Context (field :: Symbol) (s :: Type) (a :: Type) | s field -> a

instance
  (HasFieldP field (Rep s) ~ 'Just a) =>
  Context field s a

type Lens s a = forall m. Applicative m => (a -> m a) -> (s -> m s)

type MLens m s a = (a -> m a) -> (s -> m s)

class Field name s a | s name -> a where
  fieldLens :: Lens s a

instance (Field name s a, Applicative m) => IsLabel name (MLens m s a) where
  fromLabel = fieldLens @name @s @a

instance (Generic s, Context name s a, GFieldLens (Rep s) name a) => Field name s a where
  fieldLens f s = to <$> gFieldLens @_ @(Rep s) @name @a (from s) f

class GFieldLens (rep :: k -> Type) (name :: Symbol) (t :: Type) where
  gFieldLens :: Applicative m => rep x -> (t -> m t) -> m (rep x)

instance GFieldLens (_l :*: M1 S ( 'MetaSel ( 'Just name) _1 _2 _3) (K1 R t)) name t where
  gFieldLens (l :*: r) f = (l :*:) <$> gFieldLens @_ @_ @name @t r f

instance (GFieldLens l name t) => GFieldLens (l :*: _r) name t where
  gFieldLens (l :*: r) f = (:*: r) <$> gFieldLens @_ @l @name @t l f

instance GFieldLens (M1 S ( 'MetaSel ( 'Just name) _1 _2 _3) (K1 R t)) name t where
  gFieldLens (M1 (K1 v)) f = M1 . K1 <$> f v

instance (GFieldLens product name t) => GFieldLens (M1 D _dataMeta (M1 C _ctorMeta product)) name t where
  gFieldLens (M1 (M1 prod)) f = M1 . M1 <$> gFieldLens @_ @product @name @t prod f

type Getting s a = (a -> Const a a) -> (s -> Const a s)

type Setting s a = (a -> Identity a) -> (s -> Identity s)

(.^) :: s -> Getting s a -> a
(.^) s lens = getConst (lens Const s)

(.~) :: Setting s a -> a -> s -> s
(.~) lens a s = runIdentity (lens (const $ Identity a) s)

data Person = Person
  { name :: String,
    age :: Int,
    date :: String,
    address :: Maybe String
  }
  deriving stock (Show, Generic)

person1 = Person "Name" 22 "" Nothing

tmp :: Lens Person String
tmp = #name

tstLens :: Person
tstLens = person1 & #name .~ "qwe"

tstLens2 :: String
tstLens2 = person1 .^ #name

class GLabel g where
  foo :: g x -> String

-- | D - data (data type)
--   C - ctor
--   S - selector (field)
instance GLabel (M1 D _dataMeta (M1 C _ctorMeta (a :*: _b))) where
  foo _ = "product"

tst = foo (from person1)
