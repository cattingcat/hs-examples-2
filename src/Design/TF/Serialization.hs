{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Design.TF.Serialization () where

import Design.TF.FirstOrderLang
import Design.TF.Extensibility
import Extensions.DataKinds

fix :: (a -> a) -> a
fix f = f (fix f)

data Tree = Leaf String | Node String [Tree]
  deriving stock (Show, Eq, Read)

instance ExpSym Tree where
  lit n = Node "Lit" [Leaf $ show n]
  neg e = Node "Neg" [e]
  add l r = Node "Add" [l, r]

tst1 :: Tree
tst1 = add (lit 8) (neg (add (lit 1) (lit 2)))

type ErrMsg = String

safeRead :: Read t => String -> Either ErrMsg t
safeRead s = case reads s of
  [(x, "")] -> Right x
  _         -> Left $ "Read error: " ++ s

fromTree :: ExpSym repr => Tree -> Either ErrMsg repr
fromTree (Node "Lit" [Leaf s]) = lit <$> safeRead s
fromTree (Node "Neg" [e]) = neg <$> fromTree e
fromTree (Node "Add" [l, r]) = add <$> fromTree l <*> fromTree r
fromTree n = Left $ "Unknown node: " ++ show n

tst2 :: Either ErrMsg Int
tst2 = fromTree tst1

eval :: Int -> Int
eval = id

view :: String -> String
view = id

-- | Problem: We lost polymorphism
tst3 :: IO ()
tst3 = case fromTree tst1 of
  Left err -> putStrLn err
  Right r  -> do
    print $ view r
--    print $ eval r -- It doesn't work because after @view@ call type deducted to String


-- | Attempt I: forall wrapper
newtype Wrapped = Wrapped (forall repr . ExpSym repr => repr)

fromTreeWrapped :: Tree -> Either ErrMsg Wrapped
fromTreeWrapped _ = undefined -- Wrapped <$> (fromTree @r t)

tst4 :: IO ()
tst4 = case fromTreeWrapped tst1 of
  Left err -> putStrLn err
  Right (Wrapped r)  -> do
    print $ view r
    print $ eval r  -- Works with wrapped


-- | Attempt II: Tuple
newtype DoubleExp a b = DoubleExp (a, b)

instance (ExpSym repr, ExpSym repr') => ExpSym (DoubleExp repr repr') where
  lit n = DoubleExp (lit n, lit n)
  neg (DoubleExp (e1, e2)) = DoubleExp (neg e1, neg e2)
  add (DoubleExp (l1, l2)) (DoubleExp (r1, r2)) = DoubleExp (add l1 r1, add l2 r2)

tst5 :: IO ()
tst5 = case fromTree tst1 of
  Left err -> putStrLn err
  Right (DoubleExp (r1, r2))  -> do
    print $ view r1
    print $ eval r2

-- | HList
newtype ExpHList ts where
  ExpHList :: HList ts -> ExpHList ts

instance ExpSym (ExpHList '[]) where
  lit _ = ExpHList HNil
  neg _ = ExpHList HNil
  add _ _ = ExpHList HNil

--instance ExpSym t => ExpSym (ExpHList (t ': '[])) where
--  lit n = ExpHList $ HCons (lit n) HNil
--  neg (ExpHList(HCons e _)) = ExpHList $ HCons (neg e) HNil
--  add (ExpHList(HCons le _))  (ExpHList (HCons re _)) = ExpHList $ HCons (add le re) HNil

instance (ExpSym t, ExpSym (ExpHList ts)) => ExpSym (ExpHList (t ': ts)) where
  lit n = let
    (ExpHList t) = lit n
      in ExpHList $ HCons (lit n) t
  neg (ExpHList (HCons e t)) = let
    (ExpHList tl) = neg (ExpHList t)
      in ExpHList $ HCons (neg e) tl
  add (ExpHList (HCons le lt)) (ExpHList (HCons re rt)) = let
    (ExpHList tl) = add (ExpHList lt) (ExpHList rt)
      in ExpHList $ HCons (add le re) tl

tst6 :: IO ()
tst6 = case fromTree @(ExpHList [String, Int, Tree]) tst1 of
  Left err -> putStrLn err
  Right (ExpHList (HCons r1 (HCons r2 (HCons r3 HNil))))  -> do
    print $ view r1
    print $ eval r2
    print   r3


-- | Extension

instance MulSym Tree where
  mul l r = Node "Mul" [l, r]

instance (MulSym repr, MulSym repr') => MulSym (DoubleExp repr repr') where
  mul (DoubleExp (l1, l2)) (DoubleExp (r1, r2)) = DoubleExp (mul l1 r1, mul l2 r2)

--fromTreeExt' :: (ExpSym repr, MulSym repr) => Tree -> Either ErrMsg repr
--fromTreeExt' (Node "Mul" [l, r]) =  mul <$> fromTreeExt' l <*> fromTreeExt' r
--fromTreeExt' n = fromTree n -- Not correct, from tree will not parse "Mul" nodes


fromTreeExtFix' :: (ExpSym repr, MulSym repr) => (Tree -> Either ErrMsg repr) -> Tree -> Either ErrMsg repr
fromTreeExtFix' self (Node "Mul" [l, r])   = mul <$> self l <*> self r
fromTreeExtFix' _    (Node "Lit" [Leaf s]) = lit <$> safeRead s
fromTreeExtFix' self (Node "Neg" [e])      = neg <$> self e
fromTreeExtFix' self (Node "Add" [l, r])   = add <$> self l <*> self r
fromTreeExtFix' _    n                     = Left $ "Unknown node: " ++ show n

fromTreeExt' :: (ExpSym repr, MulSym repr) => Tree -> Either ErrMsg repr
fromTreeExt' = fix fromTreeExtFix'



-- | Truly extensible parser

-- Old parser
fromTreeFix1' :: (ExpSym repr) => (Tree -> Either ErrMsg repr) -> Tree -> Either ErrMsg repr
fromTreeFix1' _    (Node "Lit" [Leaf s]) = lit <$> safeRead s
fromTreeFix1' self (Node "Neg" [e])      = neg <$> self e
fromTreeFix1' self (Node "Add" [l, r])   = add <$> self l <*> self r
fromTreeFix1' _    n                     = Left $ "Unknown node: " ++ show n

-- Parser with Mul calls old one, but pass callback for new one
fromTreeFix2' :: (ExpSym repr, MulSym repr) => (Tree -> Either ErrMsg repr) -> Tree -> Either ErrMsg repr
fromTreeFix2' self (Node "Mul" [l, r])   = mul <$> self l <*> self r
fromTreeFix2' self n                     = fromTreeFix1' self n
