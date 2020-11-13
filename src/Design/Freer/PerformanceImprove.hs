{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Design.Freer.PerformanceImprove () where

import Control.Applicative (Const)
import Control.Monad ((>=>))
import Data.Coerce
import Data.Functor.Identity (Identity)
import Data.Kind
import Data.Monoid (Sum (..))
import Data.Proxy
import GHC.Natural (naturalToWord)
import GHC.TypeNats
import Unsafe.Coerce (unsafeCoerce)

type family Has (a :: k) (as :: [k]) :: Bool where
  Has a (a ': _) = 'True
  Has _ '[] = 'False
  Has a (_ ': as) = Has a as

type family NonEmpty (as :: [k]) :: Bool where
  NonEmpty '[] = 'False
  NonEmpty _ = 'True

type family IdxOf (a :: k) (as :: [k]) :: Nat where
  IdxOf a (a ': _) = 0
  IdxOf a (_ ': as) = 1 + IdxOf a as

data Any = forall a. MkAny a

type role Union phantom nominal

data Union (r :: [Type -> Type]) x = MkUnion !Word Any

class Member t r where
  inj :: t v -> Union r v
  prj :: Union r v -> Maybe (t v)

instance (Has r rs ~ 'True, IdxOf r rs ~ n, KnownNat n) => Member r rs where
  inj v = MkUnion (naturalToWord $ natVal $ Proxy @n) (MkAny v)
  prj (MkUnion indx (MkAny v)) =
    if indx == naturalToWord (natVal $ Proxy @n)
      then Just (unsafeCoerce v)
      else Nothing

decomp :: forall r rs x. Union (r ': rs) x -> Either (Union rs x) (r x)
decomp u@(MkUnion n a) =
  let res :: Maybe (r x) = prj u
   in case res of
        Nothing -> Left $ MkUnion (n - 1) a
        Just r -> Right r

tstUnion :: Union [Identity, Const Int, (,) String] Int
tstUnion = inj ("", 55)

data ArrQueue (m :: Type -> Type) a b where
  ASing :: (a -> m b) -> ArrQueue m a b
  AComp :: (a -> m b) -> ArrQueue m b c -> ArrQueue m a c

(|>) :: ArrQueue m a x -> (x -> m b) -> ArrQueue m a b
(|>) (ASing f) g = AComp f (ASing g)
(|>) (AComp f fc) g = AComp f (fc |> g)

tsingleton :: (a -> m b) -> ArrQueue m a b
tsingleton = ASing

(|><|) :: ArrQueue m a x -> ArrQueue m x b -> ArrQueue m a b
(|><|) (ASing f) g = AComp f g
(|><|) (AComp f fc) g = AComp f (fc |><| g)

type Arr r a b = a -> Eff r b

type Arrs (r :: [Type -> Type]) a b = ArrQueue (Eff r) a b

data Eff (r :: [Type -> Type]) a where
  Pure :: a -> Eff r a
  Impure :: Union r x -> Arrs r x a -> Eff r a

instance Functor (Eff r) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure a c) = Impure a (c |> (Pure . f))

instance Applicative (Eff r) where
  pure = Pure
  (Pure f) <*> (Pure a) = Pure (f a)
  (Pure f) <*> (Impure a c) = Impure a (c |> (Pure . f))
  (Impure a c) <*> (Pure b) = Impure a (c |> (Pure . ($ b)))
  (Impure a f) <*> (Impure b g) = Impure a (f |> (\ff -> Impure b (g |> (Pure . ff))))

instance Monad (Eff r) where
  (Pure a) >>= f = f a
  (Impure a g) >>= f = Impure a (g |> f)

signleK :: Arr r a b -> Arrs r a b
signleK = tsingleton

qApp :: Arrs r a b -> a -> Eff r b
qApp (ASing f) a = f a
qApp (AComp f fc) a = foo (f a) fc
  where
    foo :: Eff r a -> Arrs r a b -> Eff r b
    foo (Pure pa) arr = qApp arr pa
    foo (Impure uni iarr) arr = Impure uni (iarr |><| arr)

data Reader s a where
  Get :: Reader s s

ask :: Member (Reader s) r => Eff r s
ask = Impure (inj Get) (tsingleton Pure)

send :: Member t r => t a -> Eff r a
send ta = Impure (inj ta) (tsingleton Pure)

addGet :: Member (Reader Int) r => Int -> Eff r Int
addGet x = ask >>= (\s -> pure (x + s))

addN :: Member (Reader Int) r => Int -> Eff r Int
addN n = foldl (>=>) pure (replicate n addGet) 0

runReader :: s -> Eff (Reader s ': r) a -> Eff r a
runReader s m = loop m
  where
    loop (Pure a) = pure a
    loop (Impure u q) = case decomp u of
      Right Get -> loop $ qApp q s
      Left u -> Impure u (qComp q loop)

runEff :: Eff '[] a -> a
runEff (Pure a) = a
runEff (Impure _ _) = error "impossible"

qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arrs r' a c
qComp g h = tsingleton $ h . qApp g

tst :: Int
tst = runEff $ runReader 55 tstM
  where
    tstM :: Eff '[Reader Int] Int
    tstM = addGet 5

data Writer w a where
  Put :: w -> Writer w ()

runWriter :: forall w r a. Monoid w => Eff (Writer w ': r) a -> Eff r (w, a)
runWriter = loop
  where
    loop :: Eff (Writer w ': r) a -> Eff r (w, a)
    loop (Pure a) = pure (mempty, a)
    loop (Impure u q) = case decomp u of
      Right (Put w) ->
        let cont = qApp q ()
            r = loop cont
         in fmap (\(w', a) -> (w <> w', a)) r
      Left o -> Impure o (qComp q loop)

put :: Member (Writer w) r => w -> Eff r ()
put w = Impure (inj (Put w)) (tsingleton Pure)

rwApp :: Eff [Reader Int, Writer (Sum Int)] Int
rwApp = do
  i :: Int <- ask
  put (Sum i <> Sum i)
  _ :: Int <- if i > 0 then ask else pure 66
  ask

tstRwApp = runEff $ runWriter (runReader 999 rwApp)
