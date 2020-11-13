{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}

module Extensions.DeriveLift (Foo (..)) where

import Language.Haskell.TH.Syntax

data Foo a = Foo a | a :^: a deriving stock (Lift, Show)

-- | What is Exp
-- https://artyom.me/lens-over-tea-6
tst :: Q Exp
tst = lift (Foo (55 :: Int))
