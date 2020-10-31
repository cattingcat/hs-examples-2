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

module Lenses.OneMoreAttempt.Lens 
    ( Lens
    , Lens'
    , LensLike
    , LensLike'
    ) 
where 
import GHC.OverloadedLabels ( IsLabel(..) )
import Data.Functor.Const
import Data.Functor.Identity
import GHC.TypeLits (Symbol)
import Data.Functor ((<&>))
import Data.Functor.Contravariant 
import Data.Monoid ( First(..) )
import Data.Semigroup ( Endo(..) )
import Data.Monoid ( Any(..) )
import Data.Function ((&))
import Data.Monoid (Product(..))
import Data.Coerce (coerce)
import Data.Profunctor (Profunctor)

type Lens s t a b = forall f . Functor f => (a -> f b) -> (s -> f t)

type Lens' s a = Lens s s a a 

type LensLike f s t a b = (a -> f b) -> (s -> f t)

type LensLike' f s a = LensLike f s s a a 

type Getting r s a = LensLike' (Const r) s a

type Setting s t a b = LensLike Identity s t a b



class FieldLens (name :: Symbol) s a | s name -> a where
  fieldLens :: Lens' s a

instance (Functor f, FieldLens name s a) => IsLabel name (LensLike' f s a) where
  fromLabel = fieldLens @name @s @a

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
_last f (a:as) = (a:) <$> _last f as

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


data MyTestData = MyTestData 
    { dataId :: Int
    , dataName :: String
    , dataT :: (String, Int)
    }
    deriving stock (Show)

instance FieldLens "dataId" MyTestData Int where 
    fieldLens f d = (\i -> d{dataId = i}) <$> f (dataId d)



tstData :: MyTestData
tstData = MyTestData 23 "qwe" ("asd", 55)

foo :: Lens' MyTestData Int -> MyTestData -> Int
foo l d = unF (l f d)
    where 
        f :: Int -> Const Int Int
        f i = Const i
        unF :: Const Int MyTestData -> Int
        unF = getConst
    
tst1 = foo #dataId tstData



tstOver :: MyTestData
tstOver = over #dataId (+1) tstData

tstSet :: MyTestData
tstSet = set' #dataId (1 :: Int) tstData

tstSetIx :: [Int]
tstSetIx = set' (ix 5) 666 [1,2,3,4,5,6,7,8,9,0]

tstView :: Int
tstView = view #dataId tstData

tstNewValGet :: (Int, MyTestData)
tstNewValGet = (#dataId <%- (+1)) tstData


tstList :: [Int]
tstList =[1,2,3,4,5,6,3,7,8,9,3,7,6,5,3,5,6,7]

tstSetAll :: [Int]
tstSetAll = set' (_all 3) 666 tstList

-- Here we need monoid instance for first element of Const
-- tstViewAll = view (_all 3) tstList

tstListOf :: [Int]
tstListOf = toListOf (_all 3) tstList

tstListOf' :: [Int]
tstListOf' = toListOf' (_all 3) tstList

tstPreview :: Maybe Int
tstPreview = preview (_all 3) tstList

tstEach :: [String]
tstEach = over each (show) tstList

tstLast :: [Int]
tstLast = over _last (const 666) tstList

tstHas :: Bool
tstHas = has (_all 399) tstList

tstViewOp = tstData ^. #dataId

tstOverOp :: MyTestData
tstOverOp = tstData & #dataId %~ (+666)

tstSetOp :: (Int, Int)
tstSetOp = (0, 0) & _1 .~ 666
                  & _2 .~ 777

tstSetMonoid :: [Product Int]
tstSetMonoid =  (coerce tstList) & ix 2 <>~ (Product 666)

tstToListOp :: [Int]
tstToListOp = tstList ^.. filtered (> 6)

tstToListOpMod :: [MyTestData]
tstToListOpMod = [tstData, tstData, tstData] ^.. each

tstComp :: Int
tstComp = [[1,2,3], [4,5,6], [7,8,9]] ^. (ix 1 . ix 2)

tstComp2 :: Int
tstComp2 = [tstData, tstData, tstData] ^. (ix 1 . #dataId)

tstComp3 :: [MyTestData]
tstComp3 = [tstData, tstData, tstData] & (ix 1 . #dataId) %~ (+543)

tstComp4 :: [MyTestData]
tstComp4 = [tstData, tstData, tstData] & (each . #dataId) %~ (+543)

tstTakingSet :: [Int]
tstTakingSet = tstList & (taking 5) .~ (55 :: Int)

tstTakingGet :: [Int]
tstTakingGet = tstList ^.. taking 5

tstTo :: [String]
tstTo = tstList ^.. (each . to show)

tstAct = tstList ^! (each . to show . act putStrLn)

tstTo2 :: Int
tstTo2 = tstList ^. to length


tst = tstData ^. (#dataId . to show {-. act putStrLn-})

coerce' :: (Functor f, Contravariant f) => f a -> f b
coerce' = contramap (const ()) . fmap (const ()) 
