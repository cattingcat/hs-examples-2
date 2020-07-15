{-# LANGUAGE LambdaCase #-}

module Design.TF.PatternMatching () where
import Design.TF.FirstOrderLang

-- | Initial approach to optimizations
data Exp =
    Lit Int
  | Neg Exp
  | Add Exp Exp

pushNeg :: Exp -> Exp
pushNeg (Neg (Neg e)) = pushNeg e
pushNeg (Neg (Add l r)) = Add (pushNeg $ Neg l) (pushNeg $ Neg r)
pushNeg (Add l r) = Add (pushNeg l) (pushNeg r)
pushNeg e = e


-- | Final approach
data Ctx = P | N
newtype Optimizer repr = Optimizer {appOpt :: Ctx -> repr}

instance ExpSym repr => ExpSym (Optimizer repr) where
  lit n = Optimizer (\case {P -> lit n; N -> neg (lit n)})
  neg (Optimizer e) = Optimizer (\case {P -> e N; N -> e P})
  add (Optimizer l) (Optimizer r) = Optimizer (\ctx -> add (l ctx) (r ctx))

tst1 :: ExpSym repr => repr
tst1 = add (lit 8) (neg (add (lit 1) (lit 2)))

tst2 :: String
tst2 = appOpt tst1 P


-- | Final approah: (a + b) + c -> a + (b + c)
data AddCtx repr = ALeft repr | ARight
newtype AddOptimizer repr = AddOptimizer {appAddOpt :: AddCtx repr -> repr}

instance ExpSym repr => ExpSym (AddOptimizer repr) where
  lit n = AddOptimizer (\case (ALeft e) -> add (lit n) e; ARight -> lit n)
  neg (AddOptimizer e) = AddOptimizer (\case (ALeft l) -> add (neg $ e ARight) l; ARight -> neg (e ARight))
  add l r = AddOptimizer (\ctx -> appAddOpt l (ALeft (appAddOpt r ctx)))
