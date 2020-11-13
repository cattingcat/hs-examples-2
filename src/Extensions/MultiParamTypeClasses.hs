{-# LANGUAGE MultiParamTypeClasses #-}

module Extensions.MultiParamTypeClasses () where

class Collection c a where
  union :: c a -> c a -> c a

class RiemannHypothesis where
  assumeRH :: a -> a

isPrime :: RiemannHypothesis => Integer -> Bool
isPrime n = assumeRH True

--instance RiemannHypothesis where
--  assumeRH = id
