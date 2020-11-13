{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Design.GenericLens.Prism where

import Control.Lens
import Data.Function ((&))
import Data.Generics.Labels
import Data.Generics.Sum
import GHC.Generics

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
