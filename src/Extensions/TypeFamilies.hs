--{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Extensions.TypeFamilies () where

import GHC.Base
import GHC.Exts (TYPE)
import GHC.TypeLits

-- | Parametrically polymorphic - safeHead :: [a] -> Maybe a
--   don't care about type of *a* and don't change behavior
--
-- Ad-hoc polymorphic - class SafeHead c a where {safeHead :: c -> a}
--   sefaHead could be changed drastically dependently oh instance

-- | Part 1: Data families

-- Test it with :k or :kind
data family GMap k :: Type -> Type

data family GMap0 :: Type -> Type -> Type

data family GMap1 (k :: Type -> Type)

data family GMap2 (k :: Type)

data instance GMap Int Int = MkGMap | MkGMapI Int

newtype instance GMap2 Int = MkGMap2I String

newtype instance GMap2 String = MkGMap2S Int

testGMap2 :: a -> GMap2 a
testGMap2 = undefined

testGMap2' = testGMap2 (33 :: Word)

-- type Type = TYPE 'LiftedRep

data family DF1 :: TYPE 'IntRep

data family DF2 (r :: RuntimeRep) :: TYPE r

data family DF3 :: Type -> TYPE 'WordRep

-- ONLY newtypes with unlifted !!
newtype instance DF1 = MkDF1 Int#

newtype instance DF2 'IntRep = MkDF2I Int#

newtype instance DF2 'WordRep = MkDF2W Word#

newtype instance DF3 String = MkDF3W Word#

foo :: DF1 -> String
foo _ = ""

tstFoo = foo (MkDF1 55#)

-- PolyKinds
data family DF4 :: k

data family DF5 (a :: k) :: k

data family DF6 :: (k -> Type) -> k

newtype instance DF5 [] a = MkDF5 [a]

data family T a

data instance T Int = A

data instance T Char = B

-- doesn't work
--foo :: T a -> Int
--foo A = 1
--foo B = 2

-- use typeclass
class Foo a where footc :: T a -> Int

instance Foo Int where footc A = 1

instance Foo Char where footc B = 2

-- Test unboxed
newtype TestUnlift (a :: TYPE 'WordRep) = MkTestUnlift a

tst1 :: TestUnlift Word# -> String
tst1 = undefined

--tst2 :: TestUnlift Int# -> String
--tst2 = undefined

-- | Part 2: Type falimies
type family Elem c :: Type -- Type -> Type

type instance Elem [Int] = Int

type family ToKind (r :: RuntimeRep) :: TYPE r

type instance ToKind 'IntRep = Int#

-- | Part 3: Closed type families
type family R a where
  R (_ a) = [a]
  R a = a

type family Times (n :: Nat) (t :: Type -> Type) (z :: Type) :: Type where
  Times 0 _ z = z
  Times n t z = t (Times (n - 1) t z)

type TstTimes = Times 5 [] Int -- :kind! TstTimes

-- | Part 4: Associated
class Sized v where
  type Size v
  type Size _ = Int
  getSize :: v -> Size v
