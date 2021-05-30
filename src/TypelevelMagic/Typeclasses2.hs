{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module TypelevelMagic.Typeclasses2 () where
import Data.Kind ( Type, Constraint )

data HList (ts :: [Type]) where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

type family PairUp (as :: [Type]) where
  PairUp '[] = '[]
  PairUp (a ': b ': ts) = (a, b) ': PairUp ts


data EvenProof (ts :: [Type]) where
  EvenNil :: EvenProof '[]
  EvenCons :: EvenProof as -> EvenProof (a ': b ': as)

pairUp :: EvenProof as -> HList as -> HList (PairUp as)
pairUp EvenNil HNil = HNil
pairUp (EvenCons evenRest) (HCons a (HCons b as)) = (a, b) `HCons` pairUp evenRest as

-- ^ We need to pass proof manually, typeclasses could help:

class IsEven (as :: [Type]) where
  proof :: EvenProof as

instance IsEven '[] where
  proof = EvenNil

instance IsEven as => IsEven (a ': b ': as) where 
  proof = EvenCons proof

pairUp' :: IsEven as => HList as -> HList (PairUp as)
pairUp' = pairUp proof


tstData1 :: HList '[Int, Bool]
tstData1 = HCons 1 (HCons True HNil)

tst1 :: HList '[(Int, Bool)]
tst1 = pairUp' tstData1



-- Also we can use GADTs instead of type-family
data EvenProof2 (ts :: [Type]) (ps :: [Type]) where
  EvenNil2 :: EvenProof2 '[] '[]
  EvenCons2 :: EvenProof2 as bs -> EvenProof2 (a ': b ': as) ((a, b) ': bs)

pairUp2 :: EvenProof2 as bs-> HList as -> HList bs
pairUp2 = undefined 


-- type family returns enpty constaint even is list contains even number of items
type        IsEvenTF :: [Type] -> Constraint 
type family IsEvenTF as where
  IsEvenTF '[] = ()
  IsEvenTF (_ ': _ ': as) = IsEvenTF as



class UnitList as where
  unitList :: HList as

instance UnitList '[] where
  unitList = HNil

-- (a ~ ()) is importnant for instance deduction
instance (a ~ (), UnitList as) => UnitList (a ': as) where
  unitList = HCons () unitList

unsingleton :: HList '[a] -> a
unsingleton (HCons a HNil) = a

-- !!! instance (UnitList as) => UnitList (() ': as) where
-- head (part after =>) of instance contains non general type () 
--  so we need to move () to context (part before =>)
-- Instances deduction work from head, context doesn't matter while deduction
tst = unsingleton unitList