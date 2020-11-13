{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Extensions.DefaultSignatures () where

import GHC.Generics

class StructureDepth a where
  inspect :: a -> Int
  default inspect :: (Generic a, Depth (Rep a)) => a -> Int
  inspect a = depth (from a)

data MySum = A MySum MySum | B | C
  deriving stock (Show, Generic)
  deriving anyclass (StructureDepth)

class Depth a where
  depth :: a x -> Int

instance (Depth a, Depth b) => Depth (a :+: b) where
  depth (L1 l) = depth l
  depth (R1 r) = depth r

instance (Depth a, Depth b) => Depth (a :*: b) where
  depth (a :*: b) = max (depth a) (depth b)

instance (Depth a) => Depth (M1 _x _y a) where
  depth (M1 a) = depth a

instance Depth (K1 _1 a) where
  depth (K1 _) = 1

instance Depth U1 where
  depth U1 = 1

--foo :: IO ()
--foo = do
--  let a = from (undefined :: MySum)

tst1 = inspect (A B (A B (A B C)))

tst2 = inspect B

tst3 = inspect C
