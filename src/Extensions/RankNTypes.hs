{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Extensions.RankNTypes () where

-- | Rank-1 types
f1 :: forall a b. a -> b -> a
f1 a b = a

g1 :: forall a b. (Ord a, Eq b) => a -> b -> a
g1 a b = a

-- | Rank-2 types
f2 :: (forall a. a -> a) -> Int -> Int
f2 f = f . f . f . f

g2 :: (forall a. Eq a => [a] -> a -> Bool) -> Int -> Int
g2 f a = if f [a, a, a] a then 1 else 0

f3 :: ((forall a. a -> a) -> Int) -> Bool -> Bool
f3 f b = f id == 0

f4 :: Int -> (forall a. a -> a)
f4 0 = id
f4 n = id . f4 (n - 1)
