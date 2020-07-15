{-# LANGUAGE ImplicitParams #-}

module Extensions.ImplicitParams () where

import Data.List (sortBy)


sort' :: (?cmp :: a -> a -> Ordering) => [a] -> [a]
sort' = sortBy ?cmp

foo :: [Int] -> [Int]
foo l = let ?cmp = \_ _ -> EQ in sort' l


