{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Books.HaskellInDepth.MaybeMonad () where

import Relude hiding (MaybeT, runMaybeT)
import Prelude ()

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT $ fmap (fmap f) m

instance Applicative m => Applicative (MaybeT m) where
  pure a = MaybeT $ pure (Just a)
  MaybeT f <*> MaybeT a = MaybeT $ (<*>) <$> f <*> a

instance Monad m => Monad (MaybeT m) where
  (>>=) (MaybeT ma) fb = MaybeT $ ma >>= (\case Just a -> runMaybeT (fb a); _ -> pure Nothing)

instance Applicative m => Alternative (MaybeT m) where
  empty :: MaybeT m a
  empty = MaybeT (pure Nothing)

  (<|>) :: MaybeT m a -> MaybeT m a -> MaybeT m a
  MaybeT a <|> MaybeT b = MaybeT $ (<|>) <$> a <*> b

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift ma = MaybeT $ fmap Just ma

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO a = lift (liftIO a)
