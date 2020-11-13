{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Design.Arrows () where

import Control.Category
import Data.Profunctor
import Prelude ()

-- https://elvishjerricco.github.io/2017/03/10/profunctors-arrows-and-static-analysis.html

arr :: (Category p, Profunctor p) => (i -> o) -> p i o
arr f = dimap id f id

class Category p => PreArrow p where
  arr' :: (a -> b) -> p a b

dimap' :: PreArrow p => (i' -> i) -> (o -> o') -> p i o -> p i' o'
dimap' l r f = arr' r . f . arr' l

-- | Free profunctor for any data source
data FreeProfunctor p a b where
  FreeProfunctor :: (a -> x) -> p x y -> (y -> b) -> FreeProfunctor p a b

instance Profunctor (FreeProfunctor p) where
  dimap l r (FreeProfunctor l' f r') = FreeProfunctor (l' . l) f (r . r')

-- | Free prearrow for any profunctor
data Free p a b where
  Hom :: (a -> b) -> Free p a b
  Comp :: p x b -> Free p a x -> Free p a b

instance Profunctor p => Profunctor (Free p) where
  dimap l r (Hom f) = Hom (dimap l r f)
  dimap l r (Comp f g) = Comp (rmap r f) (lmap l g)

instance Profunctor p => Category (Free p) where
  id = Hom id
  Hom g . f = rmap g f
  Comp h g . f = Comp h (g . f)

type Arr p = Free (FreeProfunctor p)

liftArr :: p a b -> Arr p a b
liftArr f = Comp (FreeProfunctor id f id) (Hom id)

-- | Run Free structures with
runFree ::
  (Category q, Profunctor q) =>
  (forall x y. p x y -> q x y) ->
  Free p a b ->
  q a b
runFree _ (Hom g) = arr g
runFree f (Comp g h) = f g . runFree f h

runPro ::
  Profunctor q =>
  (forall x y. p x y -> q x y) ->
  FreeProfunctor p a b ->
  q a b
runPro f (FreeProfunctor l g r) = dimap l r (f g)

runArr ::
  (Category q, Profunctor q) =>
  (forall x y. p x y -> q x y) ->
  Arr p a b ->
  q a b
runArr f = runFree (runPro f)
