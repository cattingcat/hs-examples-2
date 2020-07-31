{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Design.Freer.Extensibility () where

import Data.Kind
import Data.Coerce

type role Union phantom nominal
data Union (r :: [Type -> Type]) x

class Member t r where
  inj :: t v -> Union r v
  prj :: Union r v -> Maybe (t v)

instance Member r (r ': rs) where
  inj = undefined
  prj = undefined

--instance Member r xs => Member r (x ': xs) where
--  inj = inj
--  prj = prj

decomp :: forall r rs x . Union (r ': rs) x -> Either (Union rs x) (r x)
decomp u = let res :: Maybe (r x) = prj u
  in case res of
    Nothing -> Left $ coerce u
    Just r -> Right r

data FFree r a where
  Pure   :: a   -> FFree r a
  Impure :: Union r x -> (x -> FFree r a) -> FFree r a

type Eff r a = FFree r a


data Reader s a where
  Get :: Reader s s

data Writer w a where
  Put :: w -> Writer w ()

ask :: Member (Reader s) r => Eff r s
ask = Impure (inj Get) Pure

put :: Member (Writer w) r => w -> Eff r ()
put w = Impure (inj $ Put w) Pure

tst :: Eff [Reader s, Writer w] s
tst = ask


type Arr r a b = a -> Eff r b

data Arrs r a b -- Combination of Arr