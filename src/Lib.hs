{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( someFunc
    ) where

import Data.Kind (Type)
import Control.Monad.Except
import Control.Arrow

someFunc :: IO ()
someFunc = putStrLn "someFunc"


newtype MyNewType (a :: Type -> Type) (b :: Type) = MkMyNewType (a b)
data MyData = MyDataA | MyDataB

foo :: MyData -> Word
foo MyDataA = 1

--foo :: Monad m => m _ -> m Int
--foo a = do
--  r <- a
--  return r


newtype MyKleisli err m a b = MkKyKleisli {runMyKleisli :: Kleisli (ExceptT err m) a b}
  deriving newtype Functor

--foldr (\x xs -> if x > 10 then [] else x:xs) [] [1..100]
--foldl (\xs x -> if x > 10 then [] else x:xs) [] [1..100]