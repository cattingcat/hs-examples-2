{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Lib
    ( someFunc
    ) where

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