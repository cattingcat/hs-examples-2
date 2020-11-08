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

module Lenses.OneMoreAttempt.Test () where 

import Lenses.OneMoreAttempt.Lens

import Data.Monoid (Product(..))
import Data.Coerce (coerce)
import GHC.TypeLits (Symbol)
import GHC.OverloadedLabels ( IsLabel(..) )
import Data.Functor.Const
import Data.Function ((&))

class FieldLens (name :: Symbol) s a | s name -> a where
  fieldLens :: Lens' s a

instance (Functor f, FieldLens name s a) => IsLabel name (LensLike' f s a) where
  fromLabel = fieldLens @name @s @a

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




-- | Tst Iso:

tstIso :: Ordering
tstIso = 0 ^. enum 




-- | Tst Lens and Traversals:


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















l :: [Int]
l = [1,2,3,4,5,6]

-- | Doctest integration
-- >>> foo
-- "123456"
tstDoctest :: String
tstDoctest = concatMap show l


