{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans        #-}

module Lenses.OneMoreAttempt.Lens where 
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor ((<&>))
import Data.Monoid ( First(..) )
import Data.Semigroup ( Endo(..) )
import Data.Monoid ( Any(..) )
import Data.Function ((&))
import Data.Profunctor
import Data.Tagged
import GHC.Natural

type Lens s t a b = forall f . Functor f => (a -> f b) -> (s -> f t)

type Lens' s a = Lens s s a a 

type LensLike f s t a b = (a -> f b) -> (s -> f t)

type LensLike' f s a = LensLike f s s a a 

type Getting r s a = LensLike' (Const r) s a

type Setting s t a b = LensLike Identity s t a b



ix :: Int -> Lens [a] [a] a a
ix 0 f (a:as) = f a <&> (:as)
ix n f (a:as) = ix (n - 1) f as <&> (a:) 
ix _ _ [] = error "List is too short"

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = f a <&> (, x)

_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = f a <&> (x, )

coerceLens :: Functor f => Lens s t a b -> LensLike f s t a b
coerceLens = id

-- coerceLens' :: Functor f => LensLike f s t a b -> Lens s t a b
-- coerceLens' = id

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getr setr f s = f (getr s) <&> setr s

-- opposite to view, makes lens from getter
to :: (s -> a) -> forall r . Getting r s a
to getter f s = Const . getConst $ f (getter s)


-- We don't use pure Lens types here, because we want to use 
--  those functions with Traversal

over :: Setting s t a b -> (a -> b) -> (s -> t)
over l f s = runIdentity (l (Identity . f) s)

view :: Getting a s a -> (s -> a)
view l d = getConst (l (Const . id) d)

set :: Setting s t a b -> b -> s -> t
set l b s = runIdentity $ l (const (Identity b)) s

set' :: Setting s s a a -> a -> s -> s
set' = set

type a \/ b = Either a b

choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (s1 \/ s2) (t1 \/ t2) a b
choosing l1 _  f (Left s1)  = Left <$> l1 f s1
choosing _  l2 f (Right s2) = Right <$> l2 f s2

(<%-) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%-) l f s = l (\a -> let b = f a in (b, b)) s

(<<%-) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%-) l f s = l (\a -> let b = f a in (a, b)) s

united :: Lens' s ()
united f s = f () <&> const s 


-- It is impossible to write _all with only Functor, so let's use Applicative
type Traversal s t a b = forall f . Applicative f => (a -> f b) -> (s -> f t)
type Traversal' s a = Traversal s s a a 

_all :: Eq a => a -> Traversal' [a] a
_all _ _ [] = pure []
_all a f (b:bs) = (:) <$> tmp <*> _all a f bs 
  where 
    tmp = if a == b then f b else pure b

-- It is only for List, so let's make typeclass
-- each :: Traversal [a] [b] a b
-- each _ [] = pure []
-- each f (a:as) = (:) <$> f a <*> each f as

class Each s t a b | s -> a, t -> b, s b -> a, t a -> b where 
  each :: Traversal s t a b
  default each :: (Applicative f, Traversable g, s ~ g a, t ~ g b) => LensLike f s t a b
  each = traverse

instance Each [a] [b] a b

_head, _last :: Traversal' [a] a
_head _ [] = pure []
_head f (a:as) = (:) <$> f a <*> pure as
_last _ [] = pure []
_last f [a] = (:[]) <$> f a
_last f (a:as) = (:) <$> pure a <*> _last f as

filtered :: (a -> Bool) -> Traversal' [a] a 
filtered p f [] = pure []
filtered p f (a:as) = 
  if p a 
    then (:) <$> f a <*> filtered p f as 
    else filtered p f as

taking :: Int -> Traversal' [a] a
taking 0 _ t  = pure t
taking _ _ [] = pure []
taking n f (a:as) = (:) <$> f a <*> taking (n - 1) f as

-- toListOf :: LensLike' (Const [a]) s a -> s -> [a]
toListOf :: Getting [a] s a -> s -> [a]
toListOf l s = getConst $ l (\a -> Const [a]) s

-- toListOf is slow because of list cinstruction, more fast version:
toListOf' :: Getting (Endo [a]) s a -> s -> [a]
toListOf' l s = (`appEndo` []) $ getConst $ l (\a -> Const (Endo (a:))) s

-- preview :: LensLike' (Const (First a)) s a -> s -> Maybe a
preview :: Getting (First a) s a -> s -> Maybe a
preview l s = getFirst . getConst $ l (Const . First . Just) s

has :: Getting Any s a -> s -> Bool
has l s = getAny $ getConst $ l (\_ -> Const $ Any True) s

-- | Operators:
(^.) :: s -> Getting a s a -> a
(^.) = flip view

(%~) :: Setting s t a b -> (a -> b) -> (s -> t)
(%~) = over

(+~) :: Num a => Setting s s a a -> a -> (s -> s)
(+~) l a s = s & l %~ (+ a)

(*~) :: Num a => Setting s s a a -> a -> (s -> s)
(*~) l a s = s & l %~ (* a)

(<>~) :: Monoid a => Setting s s a a -> a -> (s -> s)
(<>~) l a s = s & l %~ (<> a)

(.~) :: Setting s t a b -> b -> (s -> t)
(.~) = set

(<+-) :: Num a => Lens s s a a -> a -> s -> (a, s)
(<+-) l a s = s & l <%- (+ a)

(<<+-) :: Num a => Lens s s a a -> a -> s -> (a, s)
(<<+-) l a s = s & l <<%- (+ a)


(^..) :: s -> Getting [a] s a -> [a]
(^..) = flip toListOf

(^?) :: s -> Getting (First a) s a -> Maybe a
(^?) = flip preview




type Acting m r s a = Getting (m r) s a

type Action m s a = forall r . Acting m r s a

(^!) :: Monad m => s -> Acting m a s a -> m a
(^!) s l = getConst $ l (\a -> Const (pure a)) s

perform :: Monad m => Acting m a s a -> s -> m a
perform = flip (^!)


act :: Monad m => (s -> m a) -> Action m s a
act f g s = Const (f s >>= \a -> getConst (g a))



-- | Isomorphism:
-- Iso' s a  -  means 's' and 'a' are isomorphic
-- so we have (a -> s) and (s -> a)
-- Primitive approach:
--    type Iso s t a b = (s -> a, b -> t)
-- Less primitive approach:
--    type Iso s t a b = Functor f => (a -> f b) <-> (s -> f t)
--    where (<->) - bidirectional arrow
--
--    class Isomorphic k where
--      ... 
--    type Iso s t a b = (Isomorphic k, Functor f) => k (a -> f b) (s -> f t)

-- type Iso s t a b = forall p . Profunctor p => p a b -> p s t
type Iso' s a = Iso s s a a 

type Iso s t a b = forall p f . (Profunctor p, Functor f) => p a (f b) -> p s (f t)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

from :: Iso s t a b -> Iso b a t s
from iso = dimap (b2t iso) (fmap $ s2a iso)

enum :: Enum a => Iso' Int a
enum = iso toEnum fromEnum

data Exchange a b s t = Exchange (s -> a) (b -> t)
instance Profunctor (Exchange s t) where 
  dimap m cm (Exchange f g) = Exchange (f . m) (cm . g) 

from2 :: Iso s t a b -> Iso b a t s
from2 iso t2fs = 
  -- f :: s -> a
  -- g :: f b -> f t
  -- r :: b  --p>  f a
  let Exchange f g = iso (Exchange (\a -> a) (\fb -> fb)) 
    in dimap (runIdentity . g . Identity) (fmap f) t2fs

-- newtype Forget r a b = Forget { runForget :: a -> r }

s2a :: Iso s t a b -> (s -> a)
s2a iso s = runForget (iso f) s
  where 
    f :: Forget a a (Identity b)
    f = Forget id

b2t :: Iso s t a b -> (b -> t)
b2t iso b = runIdentity $ unTagged $ iso (Tagged (Identity b))



-- | A lens describes something isomorphic to a product with some extra context. 
--  A lens from s to a indicates there exists c such that s is isomorphic to (c, a).
--  get :: s -> a
--  put :: s -> a -> s
--  lens :: s -> (a, a -> s)

-- | On the other hand, a prism from s to a indicates there exists c such that s 
--  is isomorphic to (Either c a).
--  get :: s -> Maybe a
--  put :: a -> s
prismNat :: (Integer -> Maybe Natural, Natural -> Integer)
prismNat = (toNatural, toInteger)
  where 
    toNatural :: Integer -> Maybe Natural
    toNatural n = if n >= 0 then Just (fromInteger n) else Nothing

-- _Left :: Prism (Either a c) (Either b c) a b
    -- get_Left :: Either a b -> Maybe a
    -- get_Left (Left a) = Just a
    -- get_Left _ = Nothing

    -- put_Left :: a -> Either a b
    -- put_Left = Left

    -- modifyLeft :: (a -> b) -> Either a c -> Either b c
    -- modifyLeft f e = case get_Left e of 
    --     Nothing -> undefined -- e, impossible to return e
    --     Just a -> put_Left (f a)
get_Left :: Either a b -> Either (Either _1 b) a
get_Left (Left a) = Right a
get_Left (Right b) = Left (Right b)

put_Left :: a -> Either a _1
put_Left = Left

modifyLeft :: (a -> b) -> Either a c -> Either b c
modifyLeft f e = case get_Left e of 
    Left l -> l
    Right a -> put_Left (f a)


type Prism s t a b = forall p f . (Choice p, Applicative f) => p a (f b) -> p s (f t)
type Prism' s a = Prism s s a a

type AReview t b = Tagged b (Identity b) -> Tagged t (Identity t)

review :: AReview t b -> b -> t
review r = runIdentity . unTagged . r . Tagged . Identity

prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' put get apfa = let
  -- p (c \/ a) (c \/ f a)
  t = right' apfa

  -- p s s 
  g = dimap f1 f2 t

  -- s -> c \/ a
  f1 s = case get s of 
      Just a -> Right a
      Nothing -> Left s

  -- s \/ f a -> f s
  f2 (Left s) = pure s
  f2 (Right fa) = fmap put fa


  in g

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism put get apfb = let 
  -- p (t \/ a) (t \/ f b)
  t = right' apfb

  -- p s t
  g = dimap f1 f2 t

  -- s -> t \/ a
  f1 = get

  -- t \/ f b -> f t
  f2 = either pure (fmap put)
  in g

reviewPrism :: Prism' s a -> a -> s
reviewPrism p = runIdentity . unTagged . p . Tagged . Identity

modifyPrism :: Prism' s a -> (a -> a) -> (s -> s)
modifyPrism p f s = runIdentity $ p (Identity . f) s


data Market a b s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b s) where 
  fmap f (Market bt sEta) = Market (f . bt) (either (Left . f) Right . sEta)

instance Profunctor (Market a b) where 
  dimap lf rf (Market bt sEta) = Market (rf . bt) (either (Left . rf) Right . sEta . lf)

instance Choice (Market a b) where 
  right' :: Market a b s t -> Market a b (x \/ s) (x \/ t)
  right' (Market bt sEta) = Market (Right . bt) foo 
    where 
      foo (Left x) = Left (Left x)
      foo (Right s) = case sEta s of 
        Left t -> Left (Right t)
        Right a -> Right a

unPrism :: forall s t a b . Prism s t a b -> (b -> t, s -> Either t a)
unPrism p = (bt, sEta) 
  where 
    c :: Market a b a (Identity b) -- p a (f b)
    c = Market Identity Right
    
    r :: Market a b s (Identity t) -- p s (f t)
    r = p c

    Market bFt sEtFa = r
    bt = runIdentity . bFt
    sEta = either (Left . runIdentity) Right . sEtFa



type L p f s t a b = p a (f b) -> p s (f t)
type Lens_ s t a b = forall f . Functor f => L (->) f s t a b
type Traversal_ s t a b = forall f . Applicative f => L (->) f s t a b
type Iso_ s t a b = forall p f . (Profunctor p, Functor f) => L p f s t a b
type Prism_ s t a b = forall p f . (Choice p, Functor f) => L p f s t a b


lens2traversal :: Lens_ s t a b -> Traversal_ s t a b
lens2traversal = id

iso2prism :: Iso_ s t a b -> Prism_ s t a b
iso2prism = id

prism2Lens :: Prism_ s t a b -> Lens_ s t a b
prism2Lens = id
