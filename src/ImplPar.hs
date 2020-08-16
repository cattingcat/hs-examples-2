{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ImplPar where

import GHC.Classes
import Data.Data



instance IP "kek" String where
  ip = "puk"

instance IP "foo" String where
  ip = "bar"



foo :: forall sym . IP sym String => Proxy sym -> String
foo _ = ip @sym @_

tst1 = foo (Proxy @"kek")
tst2 = foo (Proxy @"foo")