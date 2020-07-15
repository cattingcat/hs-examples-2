{-# LANGUAGE MultiParamTypeClasses #-}

module Extensions.ConstrainedClassMethods () where

-- ConstrainedClassMethods is implied by MultiParamTypeClasses. 
class Seq s a where
  fromList :: [a] -> s a
  elem     :: Eq a => a -> s a -> Bool