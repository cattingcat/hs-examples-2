{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module TypelevelMagic.Typeclasses1 () where

class Flatten a where
  -- type FlatOf a
  flatten :: a -> [FlatOf a]

type family FlatOf a where 
  FlatOf [[a]] = FlatOf [a]
  FlatOf [a] = a

instance (FlatOf [a] ~ a) => Flatten [a] where
  -- type FlatOf [a] = a
  flatten = id 

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  -- type FlatOf [[a]] = FlatOf [a]
  flatten = flatten . concat

tst :: [Int]
tst = flatten @([[[Int]]]) [[[1,2,3], [4,5,6]], [[7]]]
