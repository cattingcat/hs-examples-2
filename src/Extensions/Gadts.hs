{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}

module Extensions.Gadts () where

-- Don't use constaint on datatype
-- data Eq a => Set' a = MkSet [a]
-- Use ctor constraints
-- data Set' a = Eq a =>  MkSet [a]

data Maybe1 a where
  Nothing1 :: Maybe1 a
  Just1 :: a -> Maybe1 a
  deriving stock (Eq, Ord)

data Person where
  Adult :: {name :: String, children :: [Person]} -> Person
  Child :: Show a => {name :: !String, funny :: a} -> Person

data Term a where
  Lit :: {val :: Int} -> Term Int
  Succ :: {num :: Term Int} -> Term Int
  IsZero :: {arg :: Term Int} -> Term Bool
  If :: {cond :: Term Bool, tru :: Term a, fls :: Term a} -> Term a
  Pair :: Term a -> Term b -> Term (a, b)

-- Requires GADTs
eval :: Term a -> a
eval (Lit i) = i
eval (Succ t) = 1 + eval t
eval (IsZero t) = eval t == 0
eval (If b e1 e2) = if eval b then eval e1 else eval e2
eval (Pair e1 e2) = (eval e1, eval e2)

data TestGadt e where
  Tpl :: (a, a) -> TestGadt (a, a)
  Intt :: Int -> TestGadt Int

testGadtFoo :: TestGadt e -> e
testGadtFoo (Tpl (a, _)) = (a, a)
testGadtFoo (Intt t) = t + 11
