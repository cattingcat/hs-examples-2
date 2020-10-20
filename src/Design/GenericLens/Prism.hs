{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Design.GenericLens.Prism where

import GHC.Generics
import Data.Generics.Sum
import Data.Generics.Labels
import Data.Function ((&))
import Control.Lens


data MyData
  = MyA String
  | MyB Int Int
    deriving stock (Generic, Show)


testData = [MyA "1", MyA "2", MyB 1 1, MyB 2 2]

foo :: [MyData] -> [String]
foo as = as ^.. each . #_MyA

bar :: Maybe String 
bar = (MyA "1") ^? (_Ctor @"MyA")

bar' :: Maybe (Int, Int)
bar' = (MyA "1") ^? (_Ctor @"MyB")