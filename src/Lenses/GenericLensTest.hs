{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PackageImports #-}

module Lenses.GenericLensTest () where

import GHC.Generics (Generic)
import Data.Generics.Labels ()
import Data.Generics.Product
import Control.Lens
import Control.Arrow


data AnotherData = AnotherData
  { anotherBool :: Bool
  , anotherInt :: Int
  , anotherString :: String
  }
  deriving stock (Generic, Show)

data TstData = TstData
  { tstId :: Int
  , tstName :: String
  , anotherData :: AnotherData
  } deriving stock (Generic, Show)


obj = TstData 1 "name" (AnotherData True 2 "another")

tst1 = obj ^. #tstId -- getter

tst2 = #tstId .~ 33 obj -- setter 1
tst3 = obj & #tstId .~ 33 -- setter 2

tst4 = obj & #tstName %~ (<> "!!!") -- modify

tst5 = obj & #anotherData %~ #anotherInt .~ 4234 -- modify nested

tst6 = obj              -- complex update
  & #tstId .~ 33
  & #tstName .~ "!!!"
  & #anotherData %~ #anotherInt .~ 4234
  
-- complex get
tst7 = obj ^. #anotherData . #anotherInt

tst8 = obj & #tstName %%~ (\s -> putStrLn s >> pure "set") 