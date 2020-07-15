{-# LANGUAGE QuantifiedConstraints #-}

module Extensions.QuantifiedConstraints () where

class (forall m. Monad m => Monad (t m)) => Trans t where
  lift :: Monad m => m a -> (t m) a
  
  
class (forall a . Eq a => Eq (f a)) => Kek f where 
  test :: f a -> Bool

instance Kek Maybe where 
  test (Just a) = True 
  
data Puk a = Puk a

instance (Eq a{-, Num a-}) => Eq (Puk a) where 
  (==) a b = True

instance Kek Puk where 
  test (Puk a) = True 