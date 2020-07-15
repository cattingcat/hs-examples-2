{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module HsSyntax.AssociatedTypes () where

import Data.Data (Proxy(..))
import GHC.Base (Word(..), Int(..), word2Int#, int2Word#, compareWord#)


class (Num (RetType a)) => MyClass a where
  type RetType a
  foo :: a -> RetType a
  showRet :: Proxy a -> RetType a -> String


bar :: ∀ a . MyClass a => a -> String
bar a = showRet (Proxy @a) $ foo a


instance MyClass Int where
  type RetType Int = Int
  foo = id
  showRet _ i = show i

instance MyClass Word where
  type RetType Word = Int
  foo (W# w) = I# (word2Int# w)
  showRet _ i = show i

--instance MyClass String where
--  type RetType String = String -- impossible because of restriction on RetType
--  foo = undefined
--  showRet = undefined

tst1 = bar (-55 ::Int)
tst2 = bar (18446700000000000000 ::Word)

w2i :: Word -> Maybe Int
w2i (W# w) = let 
  !(I# m) = (maxBound :: Int) 
  !less = compareWord# w (int2Word# m) == LT in 
    if less  
    then Just $ I# (word2Int# w) 
    else Nothing