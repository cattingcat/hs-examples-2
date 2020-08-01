{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans        #-}

module Extensions.DeriveGeneric () where

import GHC.Generics
import GHC.OverloadedLabels
import GHC.Records
import GHC.Base
import Control.Monad.Identity
import Data.Data
import Data.Function ((&))
import Data.Functor.Const


data Person = Person
  { name :: String
  , age :: Int
  , date :: String
  , address :: Maybe String
  } deriving stock (Show, Generic)

person1 = Person "Name" 22 "" Nothing


class GLabel g where
  foo :: g x -> String

-- | D - data (data type)
--   C - ctor
--   S - selector (field)
instance GLabel (M1 D _dataMeta (M1 C _ctorMeta (a :*: _b))) where
  foo _ = "product"

tst = foo (from person1)


type family  HasFieldP (field :: Symbol) (t :: k -> Type) :: Maybe Type where
  HasFieldP field (M1 D _ (M1 C _ product))                            = HasFieldP field product
  HasFieldP field (_ :*: M1 S ('MetaSel ('Just field) _ _ _) (K1 R t)) = 'Just t
  HasFieldP field (r :*: _                                           ) = HasFieldP field r
  HasFieldP field (M1 S ('MetaSel ('Just field) _ _ _) (K1 R t))       = 'Just t
  HasFieldP _     _                                                    = 'Nothing


class Context (field :: Symbol) (s :: Type) (a :: Type) | s field -> a
instance (HasFieldP field (Rep s) ~ 'Just a)
  => Context field s a



type Lens s a = forall m . Applicative m => (a -> m a) -> (s -> m s)
type MLens m s a = (a -> m a) -> (s -> m s)

class Field name s a | s name -> a where
  fieldLens :: Lens s a



instance (Field name s a, Applicative m) => IsLabel name (MLens m s a) where
  fromLabel = fieldLens @name @s @a

instance (Generic s, Context name s a, GTst (Rep s) name a) => Field name s a where
  fieldLens f s = to <$> gtst @_ @(Rep s) @name @a  (from s) f


class GTst (rep :: k -> Type) (name :: Symbol) (t :: Type) where
  gtst :: Applicative m => rep x -> (t -> m t) -> m (rep x)

instance GTst (a :*: M1 S ('MetaSel ('Just name) _1 _2 _3) (K1 R t)) name t  where
  gtst (l :*: r) f = (l :*:) <$> gtst @_ @_ @name @t  r f

instance (GTst a name t) => GTst (a :*: b) name t where
  gtst (a :*: b) f = (:*: b) <$> gtst @_ @a @name @t  a f

instance GTst (M1 S ('MetaSel ('Just name) _1 _2 _3) (K1 R t)) name t where
  gtst (M1 (K1 v)) f = M1 . K1 <$> f v


instance (GTst product name t) => GTst (M1 D _dataMeta (M1 C _ctorMeta product)) name t where
  gtst (M1 (M1 prod)) f = M1 . M1 <$> gtst @_ @product @name @t  prod f


--(^.) :: Monad m => s -> Lens s a -> (a -> m a) -> m s
--(^.) s lens f = lens f s


type Getting s a = (a -> Const a a) -> (s -> Const a s)
type Setting s a = (a -> Identity a) -> (s -> Identity s)

(.^) :: s -> Getting s a -> a
(.^) s lens = getConst (lens (\a -> Const a) s)

(.~) :: Setting s a -> a -> s -> s
(.~) lens a s = runIdentity (lens (const $ Identity a) s)

tmp :: Lens Person String
tmp = #name

tstLens :: Person
tstLens = person1 & #name .~ "qwe"

tstLens2 :: String
tstLens2 = person1 .^ #name