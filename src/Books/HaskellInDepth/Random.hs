{-# LANGUAGE ScopedTypeVariables #-}

module Books.HaskellInDepth.Random () where

import Control.Monad.Trans.Reader
import GHC.IO.Handle.Types
import GHC.IO.IOMode
import Relude (MonadIO)
import System.Random

tst :: IO ()
tst = do
  (v :: Int) <- randomIO
  putStrLn (show v)
  pure ()

type Env = String

foo :: FilePath -> IOMode -> (Handle -> ReaderT Env IO a) -> ReaderT Env IO a
foo = undefined

bar ::
  MonadIO m =>
  FilePath ->
  IOMode ->
  (Handle -> m a) ->
  m a
bar p m f = f undefined

ff :: ReaderT Env IO String
ff = undefined

tstt :: ReaderT Env IO String
tstt = bar "" undefined (\_ -> ff)
