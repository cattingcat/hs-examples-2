{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Design.Hkd () where

import GHC.Records
import Data.Kind
import Data.Functor.Identity (Identity)
import GHC.Records.Extra qualified as Z

type C :: (Type -> Type) -> Type -> Type
type family C f a where
  C Identity a = a
  C f a = f a


data MyRecord f = MyRecord
  { hello :: C f String
  , world :: C f Word
  }

--instance Z.HasField "hello" (MyRecord f) (C f String) where
instance Z.HasField "hello" (MyRecord Identity) String where


tstObj :: MyRecord Identity
tstObj = MyRecord "hell" 444

foo :: HasField "hello" o a => o -> a
foo = getField @"hello"


bar :: Z.HasField "hello" o a => o -> a
bar = Z.getField @"hello"

tst1 = foo tstObj

tst2 = bar tstObj