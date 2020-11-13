{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HsSyntax.EqualityConstraints () where

import Data.Data
import Data.Type.Equality

class (F a ~ b) => C a b where
  type F a

class (G a ~~ b) => D a b where
  type G a

instance C Int String where
  type F Int = String

--instance C Int Maybe where
--  type F Int = Maybe

instance D Maybe Int where
  type G Maybe = Int

class Test c where
  ss :: Proxy c -> String

instance (Maybe a ~~ Maybe String) => Test a where
  ss _ = "exist"

type T = Maybe Int == Maybe Int -- 'True
