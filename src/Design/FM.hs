{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Design.FM () where

-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
-- http://okmij.org/ftp/Computation/free-monad.html

data LogLevel = Info | Warn | Err
  deriving stock (Show)

type Message = String

data LoggerF next where
  Log :: LogLevel -> Message -> (() -> next) -> LoggerF next

instance Functor LoggerF where
  fmap f (Log lvl msg nxt) = Log lvl msg (f . nxt)

interpretLogger :: LoggerF a -> IO a
interpretLogger (Log lvl msg nxt) = do
  putStrLn $ show lvl ++ ": " ++ msg
  pure $ nxt ()

data RandomF next where
  Rand :: (Int, Int) -> (Int -> next) -> RandomF next

instance Functor RandomF where
  fmap f (Rand lims nxt) = Rand lims (f . nxt)

interpretRandom :: RandomF a -> IO a
interpretRandom (Rand (from, to) nxt) = do
  pure $ nxt 4

data ReadLineF next where
  Readl :: (String -> next) -> ReadLineF next

instance Functor ReadLineF where
  fmap f (Readl nxt) = Readl (f . nxt)

interpretReadLine :: ReadLineF a -> IO a
interpretReadLine (Readl nxt) = do
  line <- getLine
  pure $ nxt line

type f ~> g = forall a. f a -> g a

interpretReadLine' :: ReadLineF ~> IO
interpretReadLine' (Readl nxt) = do
  line <- getLine
  pure $ nxt line

data Free f a = Free (f (Free f a)) | Pure a

instance (Functor f) => Functor (Free f) where
  fmap f (Free fr) = Free $ fmap (fmap f) fr
  fmap f (Pure a) = Pure $ f a

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure f <*> fa = fmap f fa
  f <*> Pure fa = fmap ($ fa) f
  Free f <*> v = Free $ fmap (<*> v) f

instance (Functor f) => Monad (Free f) where
  Pure a >>= f = f a
  Free a >>= f = Free $ fmap (>>= f) a

foldFree :: (Monad m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
foldFree _ (Pure a) = pure a
foldFree u (Free f) = do
  nxt <- u f
  foldFree u nxt

liftF :: (Functor f) => f a -> Free f a
liftF fa = Free $ fmap Pure fa

type Logger a = Free LoggerF a

evalLogger :: Logger a -> IO a
--evalLogger (MkFree l) = do
--  res <- interpretLogger l
--  evalLogger res
evalLogger = foldFree interpretLogger

type Random a = Free RandomF a

evalRandom :: Random a -> IO a
evalRandom = foldFree interpretRandom

type ReadLine a = Free ReadLineF a

evalReadLine :: ReadLine a -> IO a
evalReadLine = foldFree interpretReadLine

data AppF a where
  AppLog :: Logger () -> (() -> a) -> AppF a
  AppRnd :: Random b -> (b -> a) -> AppF a
  AppReadLn :: ReadLine b -> (b -> a) -> AppF a

instance Functor AppF where
  fmap f (AppLog l nxt) = AppLog l (f . nxt)
  fmap f (AppRnd r nxt) = AppRnd r (f . nxt)
  fmap f (AppReadLn r nxt) = AppReadLn r (f . nxt)

interpretApp :: AppF a -> IO a
interpretApp (AppLog l nxt) = do
  _ <- evalLogger l
  pure $ nxt ()
interpretApp (AppRnd l nxt) = do
  r <- evalRandom l
  pure $ nxt r
interpretApp (AppReadLn r nxt) = do
  line <- evalReadLine r
  pure $ nxt line

type App a = Free AppF a

evalApp :: App a -> IO a
evalApp = foldFree interpretApp

log'' :: String -> Logger ()
log'' s = Free (Log Info s (\_ -> Pure ()))

log' :: String -> App ()
log' s = Free $ AppLog (log'' s) (\_ -> Pure ())

logB :: String -> App ()
logB s = liftF $ AppLog (liftF $ Log Info s id) id

rnd :: App Int
rnd = liftF $ AppRnd (liftF $ Rand (1, 1000) id) id

readl :: App String
readl = liftF $ AppReadLn (liftF $ Readl id) id

simpleApp :: IO ()
simpleApp = evalApp fmApp
  where
    fmApp :: App ()
    fmApp = do
      l <- readl
      n <- fmap (+ 10) rnd
      logB $ show n ++ " " ++ l
