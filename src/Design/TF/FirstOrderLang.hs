{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Design.TF.FirstOrderLang
  ( ExpSym (..),
    tst4,
  )
where

import Data.Monoid

-- | Initial embedding (via ADT)
data Exp
  = Lit Int
  | Neg Exp
  | Add Exp Exp

tst1 = Add (Lit 8) (Neg (Add (Lit 1) (Lit 2)))

eval :: Exp -> Int
eval (Lit a) = a
eval (Neg e) = - eval e
eval (Add l r) = eval l + eval r

-- | Representation type
type Repr = Int

lit' :: Repr -> Int
lit' n = n

neg' :: Repr -> Repr
neg' e = - e

add' :: Repr -> Repr -> Repr
add' l r = l + r

tst2 = add' (lit' 8) (neg' (add' (lit' 1) (lit' 2)))

-- | Initial embedding (via ADT) is more general
view :: Exp -> String
view (Lit n) = show n
view (Neg e) = "-" ++ view e
view (Add l r) = "(" ++ view l ++ " + " ++ view r ++ ")"

tst3 = view tst1

-- | Final embedding allows multiple interpretations
class ExpSym repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr

tst4 :: ExpSym repr => repr
tst4 = add (lit 8) (neg (add (lit 1) (lit 2)))

instance ExpSym Int where
  lit = id
  neg n = - n
  add l r = l + r

instance ExpSym String where
  lit = show
  neg n = "-" ++ n
  add l r = "(" ++ l ++ " + " ++ r ++ ")"

instance ExpSym Exp where
  lit = Lit
  neg = Neg
  add = Add

sumExp :: ExpSym repr => [repr] -> repr
sumExp l = appEndo (foldMap (Endo . add) l) (lit 0)

tst5 = (sumExp [tst4, tst4]) :: Int

tst6 = (sumExp [tst4, tst4]) :: String
