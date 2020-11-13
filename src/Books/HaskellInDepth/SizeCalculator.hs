{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Books.HaskellInDepth.SizeCalculator where

import Control.Monad.Trans.RWS (RWST)
import Control.Monad.Trans.RWS qualified as R
import Control.Monad.Writer
import GHC.Show (Show (..))
import Relude
import System.Directory.Extra
import System.FilePath.Posix
import System.PosixCompat
import Prelude ()

data CalcConf = CalcConf
  { path :: FilePath,
    depth :: Int,
    fileStat :: FilePath -> IO FileStatus
  }

data LogEntry = LogEntry
  { path :: FilePath,
    stat :: FileStatus
  }

instance Show LogEntry where
  show (LogEntry p _) = "LogEntry " ++ p

data CalcState = CalcState
  { currentSize :: Int,
    depth :: Int,
    maxDepth :: Int,
    maxSize :: Int,
    maxSizePath :: FilePath
  }
  deriving stock (Show)

initialState :: CalcState
initialState = CalcState 0 0 0 0 ""

initialConf :: CalcConf
initialConf = CalcConf "/Users/mark.martynov/Desktop" 10 getFileStatus

type MyApp a = RWST CalcConf [LogEntry] CalcState IO a

tellFileStat :: MonadWriter [LogEntry] m => FilePath -> FileStatus -> m ()
tellFileStat p s = tell [LogEntry p s]

incDepth :: MonadState CalcState m => m ()
incDepth = modify inc
  where
    inc :: CalcState -> CalcState
    inc s@CalcState {depth = d} = s {depth = d + 1}

decDepth :: MonadState CalcState m => m ()
decDepth = modify inc
  where
    inc :: CalcState -> CalcState
    inc s@CalcState {depth = d} = s {depth = d - 1}

currentPathSize :: MyApp Int
currentPathSize = do
  CalcConf {path, fileStat} <- ask
  stat <- liftIO $ fileStat path
  tellFileStat path stat
  let size = fromIntegral $ fileSize stat
  pure size

traverseDirectory :: MyApp ()
traverseDirectory = do
  CalcConf {path, depth} <- ask
  CalcState {depth = d} <- get

  if d > depth
    then pure ()
    else do
      content <- liftIO $ listDirectory path
      traverse_ go content
  where
    go p = local (addPathSegment p) (incDepth >> app >> decDepth)

    addPathSegment :: FilePath -> CalcConf -> CalcConf
    addPathSegment p env@CalcConf {path = pt} = env {path = pt </> p}

app :: MyApp ()
app = do
  CalcConf p d f <- ask
  state <- get

  stat <- liftIO $ f p
  if isDirectory stat
    then do
      _ <- traverseDirectory
      pure ()
    else do
      size <- currentPathSize
      let isBigger = maxSize state < size
          (maxSz, maxSzPth) = if isBigger then (size, p) else (maxSize state, maxSizePath state)
          mdpth = let CalcState {depth, maxDepth} = state in if depth > maxDepth then depth else maxDepth
      put state {currentSize = currentSize state + size, maxSize = maxSz, maxSizePath = maxSzPth, maxDepth = mdpth}

  pure ()

tst :: IO ()
tst = do
  (a, s, w) <- R.runRWST traverseDirectory initialConf initialState
  print s

--  print w
