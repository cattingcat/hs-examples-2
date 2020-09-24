{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Uniplate.TestTypeable () where

import Data.Data


data MyData = MyData Int String
  deriving stock Data

data Any = forall a . Data a => Any a

unAny :: (forall a . Data a => a -> r) -> Any -> r
unAny f (Any a) = f a

tstInt = Any (66 :: Int)
tstStr = Any ("66" :: String)
tstBool = Any (True :: Bool)

tstCast :: Maybe String
tstCast = unAny (cast @_ @String) tstStr

tstEqT :: Int
tstEqT = foo (5 :: Int) (5 :: Int) -- ("" :: String)
  where
    foo :: forall a b . (Typeable a, Typeable b) => a -> b -> Int
    foo a b = case eqT @a @b of
      Nothing -> 0
      Just Refl -> case eqT @a @Int of
        Nothing -> 0
        Just Refl -> a + b


tstTypeOf = typeOf (undefined :: Maybe String) -- Maybe String
tstTypeRep = typeRep (Proxy :: Proxy(Maybe String)) -- Maybe String
tstTyCon = typeRepTyCon tstTypeOf -- Maybe
tstTypeRepFingerprint = typeRepFingerprint tstTypeOf -- Some id