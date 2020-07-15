{-# OPTIONS_GHC -Wno-all -Wno-Weverything -Wno-compat -Wno-missing-local-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Design.TF.HigherOrderLang2 () where


class Symantics repr where
  int :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int

  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b


tst1 = add (int 1) (int 2)

tst2 = lam (\x -> add x x)

tst3 = lam (\x -> add (app x (int 1))  (int 2) )




newtype R a = R a
  deriving newtype Show

instance Symantics R where
  int = R
  add (R l) (R r) = R $ l + r

  lam f = R $ \x -> let (R t) = f (R x) in t
  app (R f) (R a) = R $ f a

evalR :: R a -> a
evalR (R a) = a


type VarCounter = Int
newtype S a = S { unS :: VarCounter -> String }

instance Symantics S where
  int n = S $ const (show n)
  add (S l) (S r) = S $ \count -> "(" ++ l count ++ " + " ++ r count ++ ")"

  lam f = S $ \count ->
    let x = "x" ++ show count
    in  "(\\" ++ x ++ " -> " ++ unS (f (S $ const x)) (succ count) ++ ")"
  app (S f) (S a) = S $ \count -> "(" ++ f count ++ " " ++ a count ++ ")"

evalS :: S a -> String
evalS (S s) = s 0