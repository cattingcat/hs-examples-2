{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Extensions.Unboxed (
  Shape(..),
  ShapeU(..),
  Shape#(..),
  Shape2#(..),
  s,
  s',
  mkRect,
  mkRect'
) where

import GHC.Prim (Int#, Double#, (*#))
import GHC.Exts (TYPE)
import GHC.Types (RuntimeRep(..), Int(I#))
--import Data.Data (DataRep(IntRep))

foo :: (# Int#, Int# #) -> (Int, Int)
foo (# a, b #) = (I# a, I# b)

bar :: (# Int# | Int# #) -> Int
bar (# l | #) = I# l
bar (# | r #) = I# r

newtype MyNewUnboxed = MkMyNewUnboxed Int#

newtype MyNewUnboxed2 :: TYPE 'IntRep where
  MkMyNewUnboxed2 :: Int# -> MyNewUnboxed2
  
newtype Identity# :: forall (r :: RuntimeRep). TYPE r -> TYPE r where
  MkIdentity# :: forall (r :: RuntimeRep) (a :: TYPE r). a -> Identity# a
  
newtype Maybe# :: forall (r :: RuntimeRep). TYPE r -> TYPE ('SumRep '[r, 'TupleRep '[]]) where
  MkMaybe# :: forall (r :: RuntimeRep) (a :: TYPE r). (# a | (# #) #) -> Maybe# a
  
data Dt = MkDt Int# Double#


baz :: Dt -> Int
baz (MkDt i _) = I# i

levPolymorphFunc :: forall (r :: RuntimeRep). TYPE r -> Maybe# (TYPE r)
levPolymorphFunc r = MkMaybe# (# r | #)

maybePrim :: forall (r :: RuntimeRep) . Maybe# (TYPE r) -> TYPE r -> TYPE r
maybePrim (MkMaybe# (# r | #))     _ = r
maybePrim (MkMaybe# (# | (# #) #)) r = r



data Shape = Rect Int Int | Circle Int
data ShapeU = RectU Int# Int# | CircleU Int#
newtype Shape# = MkShape# (# Int# | (# Int#, Int# #) #)
newtype Shape2# = MkShape2# (# Int#, (# Int# | (# #) #) #)

s :: Int -> Int -> Int 
s x y = let shape = Rect x y in
  case shape of 
    Rect a b -> a * b
    Circle r -> 3 * r * r 
    
s' :: Int -> Int -> Int 
s' (I# x) (I# y) = let shape = MkShape2# (# x, (# y | #) #) in
  case shape of 
    MkShape2# (# a, (# b | #) #) -> I# (a *# b)
    MkShape2# (# r, (#| _ #) #) -> I# (3# *# r *# r) 
    
mkRect :: Int -> Int -> Shape 
mkRect = Rect

mkRect' :: Int -> Int -> Shape2# 
mkRect' (I# x) (I# y) = MkShape2# (# x, (# y | #) #)