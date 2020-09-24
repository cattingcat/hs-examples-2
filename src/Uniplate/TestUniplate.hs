{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Uniplate.TestUniplate () where

import Data.Data
import Data.Generics.Uniplate.Operations(biplate)
import Debug.Trace
import Data.Generics.Biplate (transformOn, Biplate)
import Data.Functor.Identity


data AnotherData = AnotherData Bool Int String
  deriving stock (Data, Show)

data TstData = TstData
  { tstId :: Int
  , tstName :: String
  , anotherData :: AnotherData
  } deriving stock (Data, Show)


data AnyShow a where
  AnyShow :: [String] -> a -> AnyShow a

tstData = TstData 1231 "asd" (AnotherData False 55 "ff")

-- https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Data.html
tst1 = gfoldl (\(AnyShow log f) d -> AnyShow ("1" : log) (f d)) (AnyShow ["0"]) tstData
tst2 = let AnyShow log _ = tst1 in log



tst3 = runIdentity $ gfoldl (\(Identity f) d -> Identity (f d)) (Identity) tstData

--tst3 = transformOn biplate transString tstData
--  where
--    transString :: String -> (String)
--    transString str = ( "1" <> str)

tst4 = runIdentity $ gfoldl ap' pure' tstData
  where
    ap' :: Data a => Identity (a -> b) -> a -> Identity b
    ap' (Identity f) d = tr' d $ Identity (f newD)
      where
        newD = runIdentity $ gfoldl ap' pure' d
    pure' = Identity

modifyInternals :: forall bg sm . (Data bg, Data sm) => (sm -> sm) -> bg -> bg
modifyInternals f a = runIdentity $ gfoldl ap' pure' a
  where
    ap' :: forall a b . Data a => Identity (a -> b) -> a -> Identity b
    ap' (Identity ff) d = Identity (ff newD)
      where
        newD = case eqT @a @sm of
          Nothing   -> runIdentity $ gfoldl ap' pure' d
          Just Refl -> f d
    pure' = Identity

tst5 = modifyInternals ("!!!" <>) tstData


tr' :: Typeable a => a -> b -> b
tr' a b = trace (show (typeOf a)) b


data Trigger a = Trigger {trigger :: Bool, fromTrigger :: a}
    deriving (Read,Ord,Eq,Show)

instance Functor Trigger where
    fmap f (Trigger a b) = Trigger a $ f b

instance (Data a) => Data (Trigger a) where
    gfoldl k z (Trigger _ x) = z (Trigger True) `k` x
    gunfold k z c = k $ z $ Trigger True
    toConstr Trigger{} = conTrigger
    dataTypeOf _ = tyTrigger

conTrigger = mkConstr tyTrigger "Trigger" [] Prefix
tyTrigger = mkDataType "Data.Generics.Uniplate.Data.Instances.Trigger" [conTrigger]