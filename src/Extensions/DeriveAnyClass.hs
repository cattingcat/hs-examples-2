{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Extensions.DeriveAnyClass () where

class SPretty a where
  sPpr :: a -> String
  default sPpr :: Show a => a -> String
  sPpr = show

data Foo a = Foo a a
  deriving stock (Show)
  deriving anyclass (SPretty)

--data Bar a = Bar a a
--  deriving stock Show
--
--instance Show a => SPretty (Bar a)

class Sizable a where
  type Size a
  type Size _ = Int

data Bar = Bar
  deriving anyclass (Sizable)

doubleBarSize :: Size Bar -> Size Bar
doubleBarSize s = 2 * s
