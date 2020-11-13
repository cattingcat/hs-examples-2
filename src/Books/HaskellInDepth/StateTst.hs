{-# LANGUAGE FlexibleContexts #-}

module Books.HaskellInDepth.StateTst where

import Control.Monad.Error
import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.State
import Data.Char (isDigit, isSpace)
import Data.Foldable (traverse_)
import Data.List (groupBy)
import Data.STRef
import Relude (Alternative (..), first, second, trace)
import Safe (readMay)

type Token = String

data Exp
  = Lit Int
  | Mul Exp Exp
  | Add Exp Exp

type Stack = [Token]

type Output = [Exp]

type PState = (Stack, Output)

isEmpty :: State PState Bool
isEmpty = null <$> gets fst

notEmpty :: State PState Bool
notEmpty = not <$> isEmpty

top :: State PState Token
top = gets (head . fst)

pop :: State PState Token
pop = do
  tmp <- get
  case tmp of
    (t : ts, o) -> do
      put (ts, o)
      pure t
    _ -> error "qwe"

pop_ :: State PState ()
pop_ = () <$ pop

push :: Token -> State PState ()
push t = modify (first (t :))

whileNotEmptyAnd :: (Token -> Bool) -> State PState () -> State PState ()
whileNotEmptyAnd pred m = go
  where
    go = do
      notEmp <- notEmpty
      when notEmp $ do
        b <- (pred <$> top)
        when b (m >> go)

output :: Token -> State PState ()
output t = modify (second (builder t))
  where
    builder "+" (e1 : e2 : es) = Add e1 e2 : es
    builder "*" (e1 : e2 : es) = Mul e1 e2 : es
    builder n es = Lit (read n) : es

isOp :: String -> Bool
isOp "+" = True
isOp "-" = True
isOp _ = False

precedence :: String -> Int
precedence "+" = 1
precedence "*" = 2
precedence _ = 0

precGTE :: String -> String -> Bool
precGTE s1 s2 = precedence s1 >= precedence s2

convertToExp :: String -> Exp
convertToExp str = head $ snd $ execState shuntingYard ([], [])
  where
    tokens = reverse (tokenize str)

    shuntingYard = (traverse_ processToken tokens) >> transferRest

    processToken ")" = push ")"
    processToken "(" = transferWhile (/= ")") >> pop_
    processToken t
      | isOp t = transferWhile (`precGTE` t) >> push t
      | otherwise = output t -- number
    transfer = pop >>= output
    transferWhile predicate = whileNotEmptyAnd predicate transfer
    transferRest = transferWhile (const True)

    tokenize = groupBy (\a b -> isDigit a && isDigit b) . filter (not . isSpace)

tstData :: [(Int, String)]
tstData = [(1, "kek"), (2, "puk")]

addState :: Int -> State Int ()
addState n = modify (+ n)

foo :: Int
foo = snd $ runState (go tstData) 0
  where
    go l = traverse foo l

    foo (n, _) = addState n

countZero :: [Int] -> Word
countZero xs = runST $ do
  var <- newSTRef 0
  traverse (foo var) xs
  readSTRef var
  where
    foo ref i = do
      val <- readSTRef ref
      let newVal = if i == 0 then val + 1 else val
      writeSTRef ref newVal

type RPNState = [Int]

pushRPN :: Monad m => Int -> StateT RPNState m ()
pushRPN n = modify (n :)

popRPN :: MonadError String m => StateT RPNState m Int
popRPN = do
  s <- get
  case s of
    [] -> throwError "Pop from empty stack"
    (x : xs) -> do
      put xs
      pure x

singleRPN :: MonadError String m => StateT RPNState m Int
singleRPN = do
  s <- get
  case s of
    [x] -> put [] >> pure x
    _ -> throwError "Not single elem in stack"

readAlt :: (Read a, Alternative m) => String -> m a
readAlt s = maybe empty pure (readMay s)

evalRPN :: MonadError String m => String -> m Int
evalRPN s = fst <$> runStateT evalRPN' []
  where
    evalRPN' :: MonadError String m => StateT RPNState m Int
    evalRPN' = traverse_ processSymbol (words s) >> singleRPN

    processSymbol :: MonadError String m => String -> StateT RPNState m ()
    processSymbol "+" = ((+) <$> popRPN <*> popRPN) >>= pushRPN
    processSymbol "-" = ((-) <$> popRPN <*> popRPN) >>= pushRPN
    processSymbol "*" = ((*) <$> popRPN <*> popRPN) >>= pushRPN
    processSymbol str = case readMay str of
      Just n -> pushRPN n
      _ -> throwError $ str ++ " - Not a number"

tst :: String -> Either String Int
tst = evalRPN
