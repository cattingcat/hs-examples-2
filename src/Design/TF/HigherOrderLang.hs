{-# OPTIONS_GHC -Wno-all -Wno-Weverything -Wno-compat -Wno-missing-local-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Design.TF.HigherOrderLang () where
import Prelude hiding (lookup)


-- | Naive initial embedding
data VIndex = Vz | Vs VIndex
data Exp1 =
    Vr VIndex
  | Bl Bool
  | Lam Exp1
  | App Exp1 Exp1

data U = Ub Bool | Uf (U -> U)

eval0 :: [U] -> Exp1 -> U
eval0 env (Vr index) = lookup index env
eval0 _   (Bl val) = Ub val
eval0 env (Lam e) = Uf $ \x -> eval0 (x:env) e
eval0 env (App e1@(Lam _) e2) = case (eval0 env e1, eval0 env e2) of
  (Uf f, e2r) -> f e2r
  _ -> error "Wrong statement" -- err 1
eval0 _ _ = error ""

lookup :: VIndex -> [a] -> a
lookup Vz     (x:_)  = x
lookup (Vs n) (_:xs) = lookup n xs
lookup _      _      = error "" -- err 2



-- | Better typechack with GADTs
data Exp env t where
  B :: Bool           -> Exp env Bool
  V :: Var env t      -> Exp env t
  L :: Exp (a, env) b -> Exp env (a -> b)
  A :: Exp env (a -> b) -> Exp env a -> Exp env b

data Var env t where
  VZ :: Var (t, env) t
  VS :: Var env t -> Var (a, env) t

eval :: env -> Exp env t -> t
eval _ (B b) = b
eval env (V var) = lookUp var env
eval env (L e) = \x -> eval (x, env) e
eval env (A expL expA) = eval env expL $ eval env expA

lookUp :: Var env t -> env -> t
lookUp VZ (t, _) = t
lookUp (VS v) (_, env) = lookUp v env

tst1 :: Int
tst1 = eval (11, ()) (V VZ)

tst2 :: Bool
tst2 = eval () (A (L (V VZ)) (B True))



-- | Tagless final approach
-- Lambda calculus with indexes instead of arg names
class Symantics repr where
  int :: Int -> repr h Int
  add :: repr h Int -> repr h Int -> repr h Int

  z :: repr (a, h) a
  s :: repr h a -> repr (any, h) a

  lam :: repr (a, h) b -> repr h (a -> b)
  app :: repr h (a -> b) -> repr h a -> repr h b

--tst3 :: Symantics repr => repr h Int
tst3 = add (int 1) (int 2)

tst4 = lam (add (z) (s z))

tst5 = lam (add (app (z) (int 1)) (int 2))


newtype R ctx a = R {unR :: ctx -> a}

instance Symantics R where
  int n = R $ const n
  add (R a) (R b) = R $ \ctx -> a ctx + b ctx

  z = R fst
  s (R v) = R $ \(_, xs) -> v xs

  lam (R e) = R $ \ctx a -> e (a, ctx)
  app (R f) (R a) = R $ \ctx -> f ctx (a ctx)

evalR :: R () a -> a
evalR r = unR r ()

newtype S ctx a = S {unS :: Int -> String}

instance Symantics S where
  int n = S $ const (show n)
  add (S a) (S b) = S $ \ctx -> "(" ++ a ctx ++ " + " ++ b ctx  ++ ")"

  z = S $ \ctx -> "x" ++ show ctx
  s (S v) = S $ \ctx -> v (ctx + 1)

  lam (S f) = S $ \ctx -> "(\\x" ++ show ctx ++ " -> " ++ f (ctx) ++ ")"
  app (S f) (S a) = S $ \ctx -> "(" ++ f ctx ++ " " ++ a ctx ++ ")"

evalS :: S ctx a -> IO ()
evalS s = putStrLn $ unS s 0