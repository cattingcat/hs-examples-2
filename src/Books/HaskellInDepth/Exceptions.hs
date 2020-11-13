{-# LANGUAGE DerivingStrategies #-}

module Books.HaskellInDepth.Exceptions () where

import Control.Exception.Safe
import Relude
import Prelude ()

data MyArithException
  = DivByZero
  | OtherArithExcept
  deriving stock (Show)

instance Exception MyArithException

divSafe :: MonadThrow m => Int -> Int -> m Int
divSafe _ 0 = throwM DivByZero
divSafe a b = pure (a `div` b)

tstDiv a b c = divSafe a b >>= divSafe c

--  (tstDiv 22 0 30) :: Maybe Int

divTestIO :: Int -> Int -> Int -> IO Int
divTestIO a b c = handle handler (tstDiv a b c)
  where
    handler :: MyArithException -> IO Int
    handler e = do
      print e
      pure 88

data Level = Info | Warn | Error

newtype LoggingT m a = LoggingT {runLoggingT :: (Level -> String -> IO ()) -> m a}

class MonadLogging m where
  log :: Level -> String -> m ()

instance MonadIO m => MonadLogging (LoggingT m) where
  log lvl msg = LoggingT (\f -> liftIO $ f lvl msg)
