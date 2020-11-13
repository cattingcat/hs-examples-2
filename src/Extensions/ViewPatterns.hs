{-# LANGUAGE ViewPatterns #-}

module Extensions.ViewPatterns () where

type Typ = Int

data TypView
  = Unit
  | Arrow Typ Typ

view :: Typ -> TypView
view = undefined

foo :: Int -> Int
foo (view -> Unit) = 0
foo (view -> Arrow a b) = a

example :: (String -> Integer) -> String -> Bool
example f (f -> 4) = True
