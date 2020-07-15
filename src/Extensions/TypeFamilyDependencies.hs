{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Extensions.TypeFamilyDependencies () where

import GHC.Base


-- | r -> a  - means injectivity of type family, so second pattern matching brokes compilation
type        Id :: Type -> Type
type family Id a = r | r -> a where
  Id String = String
--  Id Int = String -- non injective