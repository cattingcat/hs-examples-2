{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypelevelMagic.Simgletons () where

import Data.Data (Proxy(..))
import Data.Kind (Type)


data Nat = Z | S Nat

data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

snat2Int :: SNat n -> Int
snat2Int SZ = 0
snat2Int (SS n) = 1 + snat2Int n

type family Sing (a :: k) :: Type
type instance Sing (n :: Nat) = SNat n

class SingI (a :: k) where
  toSing :: Proxy a -> Sing a

instance SingI 'Z where toSing _ = SZ
instance SingI n => SingI ('S n) where toSing _ = SS (toSing (Proxy @n))

tst1 = snat2Int $ toSing (Proxy @('S('S('S('S('S 'Z))))))