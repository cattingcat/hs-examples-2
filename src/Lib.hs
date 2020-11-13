{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Lib
  ( someFunc,
  )
where

import Control.Arrow
import Control.Category
import Control.Monad.Except
import Data.Kind (Type)

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
  deriving newtype (Functor)

--foldr (\x xs -> if x > 10 then [] else x:xs) [] [1..100]
--foldl (\xs x -> if x > 10 then [] else x:xs) [] [1..100]

class Category c => AMonad c m where
  apure :: c a (m a)
  abind :: c a (m b) -> c (m a) (m b)
