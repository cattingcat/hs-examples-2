{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module Lenses.Lens () where

import Data.Functor.Const
import Data.Functor.Identity
import Prelude hiding (all)

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

data Storey x f a = Storey x (f a)
  deriving stock (Show)

instance Functor f => Functor (Storey x f) where
  fmap f (Storey x fa) = Storey x (fmap f fa)

ix :: Int -> Lens' [a] a
ix 0 f (x : xs) = fmap (: xs) (f x)
ix index f (x : xs) = let rs = ix (index - 1) f xs in fmap (x :) rs
ix _ _ _ = error "err"

over :: Lens s t a b -> (a -> b) -> s -> t
over l f s = runIdentity $ l (Identity . f) s

view :: Lens s t a b -> s -> a
view l s = getConst $ l Const s

tst1 = over (ix 2) (const 100) [1, 2, 3, 4, 5, 6, 7]

tst2 = view (ix 2) [1, 2, 3, 4, 5, 6, 7]

-- | Use Storey for get AND set
--   Use Const for only get
--   Use identity for only set (w/o Functor things)
all1 :: Eq a => a -> Lens' [a] a
all1 v f [] = [] <$ f v
all1 v f (x : xs) =
  if v == x
    then
      let fa = f x
          ft = all1 v f xs
       in undefined -- impossible to concat (f a) and (f [a])
    else fmap (x :) (all1 v f xs)

type AppLens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type AppLens' s a = AppLens s s a a

appOver :: AppLens s t a b -> (a -> b) -> s -> t
appOver l f s = runIdentity $ l (Identity . f) s

all :: Eq a => a -> AppLens' [a] a
all _ _ [] = pure []
all v f (x : xs) =
  if v == x
    then
      let fa = f x
          ft = all v f xs
       in (:) <$> fa <*> ft
    else fmap (x :) (all v f xs)

-- appOver (all 5) (+10) [1,1,5,1,1,5,1,1,5]
-- appOver (all 5) (+10) []

all' :: Eq a => a -> AppLens' [a] a
all' v f s = traverse upd s
  where
    upd a = if v == a then f a else pure a

-- (all' 0) (const $ putStr "? new: " >> readLn) [100, 600, 0, 200, 0]
