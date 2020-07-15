{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}

module Lenses.Lenses () where

-- | https://artyom.me/lens-over-tea-1
-- Lenses article


-- Get and Set is different operations
data Lens1 s a = Lens1
  { getter :: s -> a
  , setter :: a -> s -> s }

ix1 :: Int -> Lens1 [a] a
ix1 n = Lens1 (getter n) (setter n) where
  getter 0 (a:_) = a
  getter i (_:xs) = getter (i - 1) xs
  setter 0 a (_:xs) = a:xs
  setter i a (x:xs) = x : setter (i - 1) a xs

--setter (ix1 5) 1 [2,2,2,2,2,2]
--getter (ix1 5) [2,2,2,2,2,2]



-- Remove getter
type Lens2 s a = (a -> a) -> s -> (a, s)

ix2 :: Int -> Lens2 [a] a
ix2 0 f (x:xs) = (x, f x : xs)
ix2 index f (x:xs) = let (r, rs) = ix2 (index - 1) f xs in (r, x:rs)
ix2 _ _ _ = error "err"

--ix2 3 id [1,2,3,4,5]
--ix2 3 (const 666) [1,2,3,4,5]



-- Multiple lists
--  possible to replace Monad -> Functor
type Lens3 s a = forall m . Monad m => (a -> m a) -> s -> (a, m s)

ix3 :: Int -> Lens3 [a] a
ix3 0 f (x:xs) = (x, fmap (:xs) (f x))
ix3 index f (x:xs) = let (r, rs) = ix3 (index - 1) f xs in (r, fmap (x:) rs)
ix3 _ _ _ = error "err"

-- ix3 3 Just [1,2,3,4,5]
-- ix3 3 (const (Just 666)) [1,2,3,4,5]
-- ix3 3 (\x -> [x, x + 10]) [1,2,3,4,5]


-- Remove tuple and replace it with storey
data Storey x f a = Storey x (f a)
  deriving stock Show
  
instance Functor f => Functor (Storey x f) where
  fmap f (Storey x fa) = Storey x (fmap f fa)

type Lens4 s a = forall f . Functor f => (a -> f a) -> s -> f s

ix4 :: Int -> Lens4 [a] a
ix4 0 f (x:xs) = fmap (:xs) (f x)
ix4 index f (x:xs) = let rs = ix4 (index - 1) f xs in fmap (x:) rs
ix4 _ _ _ = error "err"

-- ix4 3 (\x -> Storey x (Just $ x + 10)) [1,2,3,4,5]