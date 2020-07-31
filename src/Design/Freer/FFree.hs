{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Design.Freer.FFree () where

import Control.Monad ((>=>))
import Data.Kind


data FFree f a where
  Pure   :: a   -> FFree f a
  Impure :: f x -> (x -> FFree f a) -> FFree f a

instance Functor (FFree f) where
  fmap f (Pure a)       = Pure (f a)
  fmap f (Impure fa ff) = Impure fa (fmap f . ff)

instance Applicative (FFree f) where
  pure = Pure
  (Pure f) <*> (Pure a) = Pure (f a)
  (Pure f) <*> (Impure fa ff) = Impure fa (fmap f . ff)
  (Impure fa ff) <*> (Pure a) = Impure fa (fmap ($ a) . ff)
  (Impure fa ff) <*> b = Impure fa (\a -> ff a <*> b)

instance Monad (FFree f) where
  (Pure a) >>= f = f a
  (Impure a fa) >>= f = Impure a (fa >=> f)


data FReaderWriter s w a where
  Get :: FReaderWriter s w s
  Put :: w -> FReaderWriter s w ()

type RWer s w a = FFree (FReaderWriter s w) a

get :: RWer s w s 
get = Impure Get Pure

put :: w -> RWer s w ()
put a = Impure (Put a) Pure

-- | It isn't possible to add more than one effect into FFree
-- We have to extend @f@ in some way



data Lan (g :: Type -> Type) a where 
  FMap :: (x -> a) -> g x -> Lan g a
  
instance Functor (Lan g) where 
  fmap f (FMap g gx) = FMap (f . g) gx
  
data Free f a = FrPure a | Free (f (Free f a)) 

type Tst g = Free (Lan g) 
-- | Expand @Tst g@ :
--     FrPure a 
--     Free (FMap (x -> Free (Lan g) a) (g x)  )
-- 
-- Looks like FFree
 