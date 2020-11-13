{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

module Extensions.PatternSynonyms (pattern Head) where

data TypeApplication = MkApp String [TypeApplication]
  deriving stock (Show, Eq)

mkInt = MkApp "Int" []

mkArrow :: TypeApplication -> TypeApplication -> TypeApplication
mkArrow from to = MkApp "->" [from, to]

mkArr :: TypeApplication -> TypeApplication
mkArr el = MkApp "[]" [el]

--pattern EndoPattern s = MkApp "->" [MkApp s [], MkApp s []]
pattern Arrow t1 t2 = MkApp "->" [t1, t2] -- bidirect pattern

isEndo :: TypeApplication -> Bool
isEndo (Arrow a b) = a == b
isEndo _ = False

tst1 = isEndo (mkArrow mkInt mkInt)

tst2 = Arrow mkInt mkInt

pattern Head x <- x : _ -- unidirectional pattern

hd :: [a] -> a
hd (Head x) = x
hd _ = error ""

-- Make bidirectional pattern
pattern HeadC x <-
  x : _
  where
    HeadC x = [x]

pattern StrictJust a <-
  Just !a
  where
    StrictJust !a = Just a

data PosNeg = Pos Int | Neg Int

pattern Smart a <-
  Pos a
  where
    Smart a
      | a >= 0 = Pos a
      | otherwise = Neg a

pattern Point :: Int -> Int -> (Int, Int)
pattern Point {x, y} = (x, y)

pointFunc :: (Int, Int) -> Int
pointFunc (Point x y) = x + y

isZero :: (Int, Int) -> Bool
isZero (Point 0 0) = True
isZero _ = False
