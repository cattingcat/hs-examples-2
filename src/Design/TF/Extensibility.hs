{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Design.TF.Extensibility
  ( MulSym (..),
  )
where

import Design.TF.FirstOrderLang

-- | Initial embedding (via ADT)
-- We have to add new expression via modivifation
data Exp
  = Lit Int
  | Neg Exp
  | Add Exp Exp
  | Mul Exp Exp

-- | Final embedding
-- Allows to extend separately
class MulSym repr where
  mul :: repr -> repr -> repr

tst2 :: (ExpSym repr, MulSym repr) => repr
tst2 = add (lit 7) (neg (mul (lit 1) (lit 2)))

tst3 :: (ExpSym repr, MulSym repr) => repr
tst3 = mul (lit 7) tst4

instance MulSym Int where
  mul l r = l * r

instance MulSym String where
  mul l r = "(" ++ l ++ " * " ++ r ++ "+"
