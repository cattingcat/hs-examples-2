module Books.HaskellInDepth.WriterTst () where

import Control.Monad.Writer


gcd' :: Int -> Int -> Int 
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)


gcdW :: Int -> Int -> Writer [String] Int
gcdW a 0 = do {tell ["finish"]; pure a}
gcdW a b = do 
  let d = a `mod` b
  tell [show a ++ " mod " ++ show b ++ " = " ++ show d]
  gcdW b d
  
gcdWIO :: Int -> Int -> WriterT String IO Int
gcdWIO a 0 = do {liftIO $ print "finish"; pure a}
gcdWIO a b = do 
  let d = a `mod` b
  liftIO $ print $ (show a ++ " mod " ++ show b ++ " = " ++ show d)
  gcdWIO b d
  
  
tst = runWriterT (gcdWIO 123 5)