{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module HsSyntax.AssociatedData () where

import Data.Data (Proxy(..))


class MyClass a where
  data AsscData a
  foo :: a -> AsscData a
  showRet :: Proxy a -> AsscData a -> String

bar :: AsscData Int -> String
bar (MkInt i) = show i
bar (MkWord i) = show i
--bar (MkStr i) = show i
--bar (MkChar i) = show i


instance MyClass Int where
  data AsscData Int = MkInt Int | MkWord Word
    deriving stock Show
  foo i = if i < 0 then MkInt i else MkWord 0
  showRet _ (MkInt i) = show i
  showRet _ (MkWord i) = show i

instance MyClass String where
  data AsscData String = MkStr String | MkChar Char
  foo = undefined
  showRet = undefined