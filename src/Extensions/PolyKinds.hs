{-# LANGUAGE PolyKinds #-}

module Extensions.PolyKinds () where
-- | See also TypeInType
--
-- w/o PolyKinds :k = (* -> *) -> * -> *
-- w/  PolyKinds :k = (k -> *) -> k -> *

newtype App f a = MkApp (f a)

newtype T a (b :: k) c = MkT (a c)