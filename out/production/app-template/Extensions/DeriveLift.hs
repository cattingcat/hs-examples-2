{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}

module Extensions.DeriveLift (Foo(..)) where
import Language.Haskell.TH.Syntax

data Foo a = Foo a | a :^: a deriving stock (Lift, Show)

