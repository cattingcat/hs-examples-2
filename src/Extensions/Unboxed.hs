{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Extensions.Unboxed () where

import GHC.Prim (Int#, Double#)
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