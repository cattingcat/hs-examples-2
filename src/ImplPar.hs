{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ImplPar where

import Data.Data
import GHC.Classes

instance IP "kek" String where
  ip = "puk"

instance IP "foo" String where
  ip = "bar"

foo :: forall sym. IP sym String => Proxy sym -> String
foo _ = ip @sym @_

tst1 = foo (Proxy @"kek")

tst2 = foo (Proxy @"foo")
