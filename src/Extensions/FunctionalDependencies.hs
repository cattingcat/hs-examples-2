{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Extensions.FunctionalDependencies () where

--class Coll1 s a where
--  empty1  :: s            -- couldn't deduct
--  insert1 :: s -> a -> s

class Coll s a | s -> a where
  empty :: s
  insert :: s -> a -> s

class Collects e ce where
  --    empty  :: ce
  insert2 :: e -> ce -> ce
  member2 :: e -> ce -> Bool

-- Problem: check f type via GHCi with and without "| ce -> e"
f a b = insert2 a . insert2 b

--g = f 'a' True
