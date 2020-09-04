{-# LANGUAGE DerivingStrategies #-}

module CodeWars.BefungeInterpreter (interpret) where

-- | https://www.codewars.com/kata/526c7b931666d07889000a3c

import Data.Functor.Identity
import Prelude
import System.Random
import Data.Char (chr, ord, digitToInt)
import Debug.Trace (trace)


-- | Lets try to make it from scratch

-- | 1. Lets define my own state monad
newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

-- | Functor instance now
--  I use Tuple functor instance btw
instance Functor m => Functor (StateT s m) where
  fmap f (StateT g) = StateT (fmap (fmap f) . g)

-- | Applicative
instance Monad m => Applicative (StateT s m) where
  pure a = StateT (\s -> pure (s, a))
  (StateT f) <*> (StateT a) = StateT runS
    where
      runS s = do
        (s', f') <-  f s
        (s'', a') <- a s'
        pure (s'', f' a')

-- | The most important: Monad
instance Monad m => Monad (StateT s m) where
  return = pure
  (StateT a) >>= f = StateT runS
    where
      runS s = do
        (s', a') <- a s
        runStateT (f a') s'

get :: Applicative m => StateT s m s
get = StateT (\s -> pure (s, s))

put :: Applicative m =>  s -> StateT s m ()
put s = StateT (\_ -> pure (s, ()))

-- | Alias
type State s a = StateT s Identity a

-- | Data structure for our state
data Direction = L | R | U | D
  deriving (Show, Enum, Bounded)
data InterprData = InterprData
  { stdGen :: StdGen
  , stack :: [Int]
  , direction :: Direction
  , stringMode :: Bool
  , output :: String
  , position :: (Int, Int)
  , finished :: Bool
  , skipNextCmd :: Bool
  }

initialState :: StdGen -> InterprData
initialState gen = InterprData gen [] R False "" (0, 0) False False

push :: Monad m => Int -> StateT InterprData m ()
push n = do
  dt <- get
  put dt{stack = n : stack dt}

pop :: Monad m => StateT InterprData m (Maybe Int)
pop = do
  dt <- get
  case stack dt of
    []     -> pure Nothing
    (x:xs) -> do
      put dt{stack = xs}
      pure (Just x)

tell' :: Monad m => String -> StateT InterprData m ()
tell' str = do
  dt <- get
  put dt{output = output dt ++ str}

finish :: Monad m => StateT InterprData m ()
finish = do
  dt <- get
  put dt{finished = True}

binOp :: Monad m => (Int -> Int -> Int) -> StateT InterprData m ()
binOp f = do
  a <- pop
  b <- pop
  case (a, b) of
    (Just a', Just b') -> push (b' `f` a')
    _ -> error "binOp empty stack"

divOp :: Monad m => (Int -> Int -> Int) -> StateT InterprData m ()
divOp f = do
  a <- pop
  b <- pop
  case (a, b) of
    (Just a', Just b') -> push (if a' == 0 then 0 else b' `f` a')
    _ -> error "divOp empty stack"

add, sub, mul, div', mod' :: Monad m => StateT InterprData m ()
add = binOp (+)
sub = binOp (-)
mul = binOp (*)
div' = divOp Prelude.div
mod' = divOp Prelude.mod

not' :: Monad m => StateT InterprData m ()
not' = do
  v <- pop
  case v of
    Just a -> push (if a == 0 then 1 else 0)
    _ -> error "not' empty stack"

cmp :: Monad m => StateT InterprData m ()
cmp = do
  a <- pop
  b <- pop
  case (a, b) of
    (Just a', Just b') -> push (if b' > a' then 1 else 0)
    _ -> error "cmp empty stack"

mv :: Monad m => Direction -> StateT InterprData m ()
mv d = do
  dt <- get
  put dt{direction = d}

mvL, mvR, mvU, mvD :: Monad m => StateT InterprData m ()
mvL = mv L
mvR = mv R
mvU = mv U
mvD = mv D

mvRnd :: Monad m => StateT InterprData m ()
mvRnd = do
  dt <- get
  let (i, gen) = next (stdGen dt)
  put (dt{stdGen = gen, direction = toEnum $ i `Prelude.mod` 4})

mvHVar :: Monad m => StateT InterprData m ()
mvHVar = do
  v <- pop
  case v of
    Just a -> if a == 0 then mvR else mvL
    _ -> error "Empty stack: mvHVar"

mvVVar :: Monad m => StateT InterprData m ()
mvVVar = do
  v <- pop
  case v of
    Just a -> if a == 0 then mvD else mvU
    _ -> error "Empty stack: mvVVar"


setStrMode :: Monad m => Bool -> StateT InterprData m ()
setStrMode mode = do
  dt <- get
  put dt{stringMode = mode}

isStrMode :: Monad m => StateT InterprData m Bool
isStrMode = stringMode <$> get

beginStrMode, endStrMode :: Monad m => StateT InterprData m ()
beginStrMode = setStrMode True
endStrMode   = setStrMode False

duplicateTop :: Monad m => StateT InterprData m ()
duplicateTop = do
  v <- pop
  case v of
    Just a  -> push a >> push a
    Nothing -> push 0

swapTop2 :: Monad m => StateT InterprData m ()
swapTop2 = do
  v1 <- pop
  v2 <- pop
  case (v1, v2) of
    (Just a, Just b)  -> push a >> push b
    (Just a, Nothing) -> push a >> push 0
    _                 -> error "swapTop2 stack is empty"

takeCoord :: Monad m => StateT InterprData m (Int, Int)
takeCoord = do
  v1 <- pop
  v2 <- pop
  case (v1, v2) of
    (Just y, Just x)  -> pure (x, y)
    _                 -> error "swapTop2 stack is empty"

discardTop :: Monad m => StateT InterprData m ()
discardTop = pop >> pure ()

outInt :: Monad m => StateT InterprData m ()
outInt = do
  v <- pop
  case v of
    Just a -> tell' (show a)
    _      -> error "outInt empty stack"

outASCII :: Monad m => StateT InterprData m ()
outASCII = do
  v <- pop
  case v of
    Just a -> tell' [chr a]
    _      -> error "outASCII empty stack"

moveCursor :: Monad m => StateT InterprData m ()
moveCursor = do
  dt <- get
  let
    (x, y) = position dt
    newData =
        case direction dt of
          L -> dt{position = (x - 1, y)}
          R -> dt{position = (x + 1, y)}
          U -> dt{position = (x, y - 1)}
          D -> dt{position = (x, y + 1)}
  put newData

setSkipNextCmd :: Monad m => Bool -> StateT InterprData m ()
setSkipNextCmd b = do
  dt <- get
  put (dt{skipNextCmd = b})

checkAndRestoreSkip :: Monad m => StateT InterprData m Bool
checkAndRestoreSkip = do
  dt <- get
  let skip = skipNextCmd dt
  setSkipNextCmd False
  pure skip


popXYV :: Monad m => StateT InterprData m (Int, Int, Int)
popXYV = do
  yv <- pop
  xv <- pop
  vv <- pop
  case (xv, yv, vv) of
    (Just x, Just y, Just v) -> pure (x, y, v)
    _                        -> error "Empty stack at popXYV"




newtype CodeField = CodeField [String]
  deriving Show

getAt :: (Int, Int) -> CodeField -> Char
getAt (x, y) (CodeField a) = let
  l = a !! (y `Prelude.mod` length a)
  in
    l !! (x `Prelude.mod` length l)


modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (x:xs) = f x : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []

setAt :: (Int, Int) -> Char -> CodeField -> CodeField
setAt (x, y) val (CodeField a) = let
  normY = y `Prelude.mod` length a
  line = a !! normY
  normX = x `Prelude.mod` length line
  in CodeField (modifyAt normY (modifyAt normX (const val)) a)

makeStep :: Monad m => CodeField -> StateT InterprData m CodeField
makeStep field = do
  dt@(InterprData g s d sm o p fin _) <- get
  skip <- checkAndRestoreSkip
  if skip
  then moveCursor >> pure field
  else
    if fin
    then pure field
    else do
      let c = getAt p field

--      trace (show c ++ "   " ++ show s) (pure ())

      isSm <- isStrMode
      if isSm then (if c == '"' then endStrMode else push (ord c)) >> moveCursor >> pure field
      else case c of
        '"' -> beginStrMode >> moveCursor >> pure field
        'p' -> do
          (x, y, v) <- popXYV
          moveCursor >> pure (setAt (x, y) (chr v) field)
        other -> mapSymbolToCommand field other >> moveCursor >> pure field




mapSymbolToCommand :: Monad m => CodeField -> Char -> StateT InterprData m ()
mapSymbolToCommand field c = case c of
  digit | digit <= '9' && digit >= '0' -> push (digitToInt digit)
  '+' -> add
  '-' -> sub
  '*' -> mul
  '/' -> div'
  '%' -> mod'
  '!' -> not'
  '`' -> cmp
  '>' -> mvR
  '<' -> mvL
  '^' -> mvU
  'v' -> mvD
  '?' -> mvRnd
  '_' -> mvHVar
  '|' -> mvVVar
  ':' -> duplicateTop
  '\\' -> swapTop2
  '$' -> discardTop
  '.' -> outInt
  ',' -> outASCII
  '#' -> setSkipNextCmd True
  'p' -> error "Implement me: p"
  'g' -> do
   coord <- takeCoord
   push (ord (getAt coord field))
  '@' -> finish
  ' ' -> pure ()
  uch -> error ("Unknown command: " ++ [uch])


run :: Monad m => CodeField -> StateT InterprData m String
run field = do
  newField <- makeStep field
  dt <- get
  if finished dt
  then pure (output dt)
  else run newField


interpret :: StdGen -> String -> String
interpret gen progStr = snd $ runIdentity (runStateT (run (CodeField (lines progStr))) (initialState gen))


prog1 = ">987v>.v\nv456<  :\n>321 ^ _@"
prog2 = ">25*\"!dlroW olleH\":v\n" ++ "                v:,_@\n" ++ "                >  ^"
prog3 = "2>:3g\" \"-!v\\  g30          <\n" ++ " |!`\"&\":+1_:.:03p>03g+:\"&\"`|\n" ++ " @               ^  p3\\\" \":<\n" ++ "2 2345678901234567890123456789012345678"

tstProg :: String -> IO ()
tstProg pr = putStrLn (interpret (mkStdGen 44) pr)


