{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HsSyntax.Compose where

import Data.Kind

type Compose :: (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype Compose f g a = Compose (f (g a))

type (:.:) f g = Compose f g

class MonoidT f where
  join :: (f :.: f) a -> f a

instance Monad m => MonoidT m where
  join (Compose mm) = mm >>= id

tst1 = join $ Compose [[1, 2, 3], [4, 5, 6]]
