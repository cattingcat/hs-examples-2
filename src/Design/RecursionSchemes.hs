{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Design.RecursionSchemes () where
import Prelude ()
import Control.Arrow
import Relude
import Data.List (partition)
import qualified System.Random as Random


-- https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

data Expr a
  = Literal { intVal :: Int }
  | Ident   { name :: String  }
  | Index   { target :: a, idx :: a }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving stock (Show, Eq, Functor)

-- | Type-level fixed point operator
newtype Term f = In { out :: f (Term f) }

ten, add, call :: Term Expr
ten = In (Literal {intVal = 10})
add = In (Ident {name = "add"})
call = In (Call {func = add, args = [ten, ten]})


-- | No intermediate state between fn calls
-- so it is impossible to calculate nodes for example
bottomUp, upBottom :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn = out >>> fmap (bottomUp fn) >>> In >>> fn
upBottom fn = fn >>> out >>> fmap (bottomUp fn) >>> In


-- https://blog.sumtypeofway.com/posts/recursion-schemes-part-2.html

mystery :: Functor f => (f a -> a) -> Term f -> a
mystery fn = out >>> fmap (mystery fn) >>> fn

countNodes :: Expr Int -> Int
countNodes (Unary _ arg)         = arg + 1
countNodes (Binary left _ right) = left + right + 1
countNodes (Call fn args)        = fn + sum args + 1
countNodes (Index it idx)        = it + idx + 1
countNodes (Paren arg)           = arg + 1
countNodes (Literal _) = 1
countNodes (Ident   _) = 1

tst = mystery countNodes call


type Algebra f a = f a -> a
--countNodes :: Algebra Expr Int


cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f

prettyPrint :: Expr String -> String
prettyPrint (Unary op arg)         = op <> " " <> arg
prettyPrint (Binary left op right) = left <> " " <> op <> " " <> right
prettyPrint (Call fn args)        = fn <> "(" <> foldl' (\b a -> if null b then a else b <> ", " <> a) "" args <> ")"
prettyPrint (Index it idx)        = it <> "[" <> idx <> "]"
prettyPrint (Paren arg)           = "(" <> arg <> ")"
prettyPrint (Literal n) = show n
prettyPrint (Ident   name) = name

tst2 = cata prettyPrint call

bottomUp' f = cata (In >>> f)


-- | Reverse cata (out -> In, change >>> direction):
reverseCata f = In <<< fmap (reverseCata f) <<< f

type Coalgebra f a = a -> f a

ana :: (Functor f) => Coalgebra f a -> a -> Term f
ana f = In <<< fmap (ana f) <<< f

nested :: Int -> Term Expr
nested n = ana go n where
  go :: Coalgebra Expr Int
  go 0 = Literal n
  go n = Paren (n - 1)

tst3 = cata prettyPrint (nested 5)




-- https://blog.sumtypeofway.com/posts/recursion-schemes-part-3.html
-- | Para contains original state (name from parallel)
type RAlgebra f a = f (Term f, a) -> a
para :: (Functor f) => RAlgebra f a -> Term f -> a
para f = out >>> fmap g >>> f
  where
--    g t = (t, para f t)
    g = id &&& para f

-- | Replace tuple with function with two params
type RAlgebra' f a = Term f -> f a -> a
para' :: (Functor f) => RAlgebra' f a -> Term f -> a
para' f t = out t & fmap (para' f) & f t


fastPretty :: RAlgebra' Expr String
fastPretty _ (Literal i) = show i
fastPretty _ (Ident s)   = s
fastPretty (In (Call (In (Ident "id")) _)) (Call {args = [theArg]}) = theArg
fastPretty _ (Call f args) = f <> "(" <> foldl' (\b a -> if null b then a else b <> ", " <> a) "" args <> ")"
fastPretty _ (Unary op arg)         = op <> " " <> arg
fastPretty _ (Binary left op right) = left <> " " <> op <> " " <> right
fastPretty _ (Index it idx)        = it <> "[" <> idx <> "]"
fastPretty _ (Paren arg)           = "(" <> arg <> ")"

callId :: Term Expr
callId = In (Call (In (Ident "id")) [call])

tstCallIdOrig = cata prettyPrint callId
tstCallIdFast = para' fastPretty callId


-- | Let's dualize RAlgebra:
type None f a = a -> f (Term f, a)
-- it doesn't work. We have to revers all arrows (categorical sum and products too)
--  dual to tuple is Either

type RCoalgebra f a = a -> f (Either (Term f) a)

-- | Apomorphism:
apo :: Functor f => RCoalgebra f a -> a -> Term f
apo f = In <<< fmap (id ||| apo f) <<< f



-- https://blog.sumtypeofway.com/posts/recursion-schemes-part-4.html

-- | Histomorphism
data Attr f a = Attr
  { attribute :: a          -- carried value, in-progress value of folds
  , hole :: f (Attr f a)    -- fixed point value
  }

type CVAlgebra f a = f (Attr f a) -> a

histo' :: Functor f => CVAlgebra f a -> Term f -> a
histo' alg = out >>> fmap foo >>> alg
  where
    foo t = Attr (histo alg t) (fmap foo (out t))

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = worker >>> attribute where
  worker = out >>> fmap worker >>> (h &&& id) >>> uncurry Attr

data NatF a = Zero | Next a
  deriving stock Functor

int2Nat :: Int -> Term NatF
int2Nat 0 = In Zero
int2Nat n = In (Next (int2Nat (n - 1)))

nat2Int :: Term NatF -> Int
nat2Int (In Zero) = 0
nat2Int (In (Next n)) = 1 + nat2Int n

fib n = histo foo (int2Nat (n - 1))
  where
    foo Zero = 1
    foo (Next (Attr n Zero)) = n
    foo (Next (Attr n (Next (Attr m _)))) = n + m


type Cent = Int

coins :: [Cent]
coins = [50, 25, 10, 5, 1]

compress :: NatF (Attr NatF a) -> Int
compress Zero              = 0
compress (Next (Attr _ x)) = 1 + compress x

change :: Cent -> Int
change amt = histo go (int2Nat amt)
  where
    go Zero = 1
    go curr@(Next attr) = let
      given = compress curr
      validCoins = filter (<= given) coins
      remainig = map (given -) validCoins
      (zeroes, toProcess) = partition (== 0) remainig
      results = sum (map (lookup attr) toProcess)
      in length zeroes + results

lookup :: Attr NatF a -> Int -> a
lookup cache 0 = attribute cache
lookup cache n = lookup inner (n - 1)
  where
    (Next inner) = hole cache

-- | Futumorphism
--  co-histomorphism
data CoAttr f a = Automatic a | Manual (f (CoAttr f a))

type CVCoalgebra f a = a -> f (CoAttr f a)

futu :: Functor f => CVCoalgebra f a -> a -> Term f
futu alg = In <<< fmap foo <<< alg
  where
    foo (Automatic a) = futu alg a
    foo (Manual a) = In (fmap foo a)


-- example:
data Plant a
  = Root a     -- every plant starts here
  | Stalk a    -- and continues upwards
  | Fork a a a -- but can trifurcate at any moment
  | Bloom      -- eventually terminating in a flower
    deriving (Show, Functor)

data Action
  = Flower  -- stop growing now
  | Upwards -- grow up with a Stalk
  | Branch  -- grow up with a Fork

data Seed = Seed
    { height :: Int
    , rng    :: Random.StdGen
    }

grow :: Seed -> (Action, Seed, Seed)
grow seed@(Seed h rand) = (choose choice, left { height = h + 1}, right { height = h + 1})
  where (choice, _) = Random.randomR (1 :: Int, 5) rand
        (leftR, rightR) = Random.split rand
        left = Seed h leftR
        right = Seed h rightR
        choose 1 = Flower
        choose 2 = Branch
        choose _ = Upwards

sow :: CVCoalgebra Plant Seed
sow seed =
  let (act, left, right) = grow seed
  in case (act, height seed) of
    (_, 0)       -> Root (Automatic left)
    (_, 10)      -> Bloom
    (Flower, _)  -> Bloom
    (Upwards, _) -> Stalk (Automatic right)
    (Branch, _)  -> Fork (Manual (Stalk (Automatic left)))
                         (Manual Bloom)
                         (Manual (Stalk (Automatic right)))

tstFutu :: IO ()
tstFutu = do
  rnd <- Random.newStdGen
  let ourPlant = futu sow (Seed 0 rnd)
  undefined --  ourPlant






-- | Futumorphism (co-histomorphism)
--   Similar to free monad
-- data CoAttr f a = Automatic a | Manual (f (CoAttr f a))
-- data Free   f a = Pure a      | Impure (f (Free f a))

-- | Histomorphism
--   Similar to co-free monad
-- data Attr   f a = Attr{ attribute :: a,     hole :: f (Attr f a) }
-- data Cofree f a =                    a :<          (f (Cofree f a))


-- https://blog.sumtypeofway.com/posts/recursion-schemes-part-5.html


-- | Hylomorphisms
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coalg = ana coalg >>> cata alg

-- | Chronomorphism
chrono :: Functor f => CVAlgebra f b -> CVCoalgebra f a -> a -> b
chrono cvalg cvcoalg = futu cvcoalg >>> histo cvalg









-- | How to live with default functors (lists from Prelude) ?
--  type family Base t :: Type -> Type
--  type instance Base [a] = ListF a
--  
--  class (Functor (Base t)) => Recursive t where
--    project :: t -> Base t t
--    cata    :: (Base t a -> a) -> t -> a

-- https://blog.sumtypeofway.com/posts/recursion-schemes-part-6.html