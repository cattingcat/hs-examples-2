{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
module Extensions.TH.THLenses (
  map2TH,
  createMap2Func,
  fstLensTH,
  fstOfNLensTH,
  myQuoter,

  listFields',
  typeName
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Lenses.Composability


--map2 :: (a -> b) -> [a] -> [b]
--map2 f (x : xs) = f x : map2 f xs
--map2 _ [] = []


arrow :: Type -> Type -> Type
arrow a b = AppT (AppT ArrowT a) b

map2TH :: [Dec]
map2TH = [
  SigD funcName (forallAB [] map2Type),
  FunD funcName [
    Clause [funcNamePat, listDeconstrPat] body1 [],
    Clause [wildCardPat, emptyListPat]    body2 [] ] ]
  where
    funcName = mkName "map2"
    -- type decl
    aName = VarT $ mkName "a"
    bName = VarT $ mkName "b"
    forallAB = ForallT [PlainTV (mkName "a"), PlainTV (mkName "b")]
    mkArr name = AppT ListT name
    farrow = aName `arrow` bName
    arrayArrow = mkArr aName `arrow` mkArr bName
    map2Type = farrow `arrow` arrayArrow
    -- body pattern 1
    fName = mkName "f"
    xName = mkName "x"
    xsName = mkName "xs"
    funcNamePat = VarP fName
    listDeconstrPat = InfixP (VarP xName) '(:)  (VarP xsName)
    fxE = AppE (VarE fName) (VarE xName)
    map2E = AppE (AppE (VarE funcName) (VarE fName)) (VarE xsName)
--    body1 = NormalB $ AppE (AppE (ConE '(:)) fxE) map2E
    body1 = NormalB $ InfixE (Just fxE) (ConE '(:)) (Just map2E)
    -- body pattern 2
    emptyListPat = ConP '[] []
    wildCardPat = WildP
    body2 = NormalB (ConE '[])

-- | Look at usages in TemplateHaskellConsumerModule

createMap2Func :: DecsQ
createMap2Func = pure map2TH


tst :: [Dec] -> IO ()
tst = putStrLn . pprint



-- | Oxford brackets
declarationBrackets :: Q [Dec] -- Multiple declarations
declarationBrackets = [d|
    map2 :: (a -> b) -> [a] -> [b]
    map2 f (x:xs) = f x : map2 f xs
    map2 _ [] = []
  |]

exprBrackets :: Q Exp  -- Expressions
exprBrackets = [e| (a + 3) * 4 |]

typeBrackets :: Q Type -- Type sig
typeBrackets = [t| forall a . Int -> a -> [a] |]

patternBrackets :: Q Pat -- PAttern matching
patternBrackets = [p| (a, x:xs, _) |]

-- | Custom parser-function
--    Specific method depends on syntactic ctx
myQuoter :: QuasiQuoter
myQuoter = QuasiQuoter {
    quoteExp  = compileExp,
    quotePat  = err "pat",
    quoteType = err "typ",
    quoteDec  = err "dec"
  }
  where
    compileExp :: String -> Q Exp
    compileExp s = if s == "+"
      then pure $ UInfixE (LitE (IntegerL 2)) (VarE '(+)) (LitE (IntegerL 2)) -- 2+2 viaADT
      else [e| 2 * 2 |] -- 2*2 via build-in parser

    err :: String -> String -> a
    err thing s = error $ thing ++ " doesnt work with: " ++ s


-- | Lift
-- Also it is possible to translate your DSL to template-haskell DSL
--  via definig List instance explicitly
data MyLang = Kek | Puk MyLang
  deriving stock Lift

myLangExp :: Q Exp
myLangExp = [e| Puk (Puk Kek) |]




printQ :: Ppr a => Q a -> IO ()
printQ q = do
  decs <- runQ q
  _ <- putStrLn . pprint $ decs
  pure ()


-- | Using interpolated quotes
mkVars :: String -> Q (PatQ, ExpQ)
mkVars name = do
  n <- newName name
  pure (varP n, varE n)

mkVars2 :: String -> Q (TypeQ, PatQ, ExpQ)
mkVars2 name = do
  n <- newName name
  pure (varT n , varP n, varE n)

fstLensTH :: String -> DecsQ
fstLensTH name = do
  let
    lensName = mkName name
    typ = [t| forall a b x . Lens (a, x) (b, x) a b |]

  sig <- sigD lensName typ

  -- (pattern, variable)
  (fp, fv) <- mkVars "f"
  (ap, av) <- mkVars "a"
  (bp, bv) <- mkVars "b"
  (xp, xv) <- mkVars "x"

  body <- funD lensName [
      clause [fp, tupP [ap, xp]] (normalB [e| (\ $bp -> ($bv, $xv)) <$> $fv $av |]) []
    ]
  pure [sig, body]

fstOfNLensTH :: String -> Int -> DecsQ
fstOfNLensTH name n = do
  let
    lensName = mkName name
    names = fmap (\x -> mkName $ "x" ++ show x) [1 .. (n - 1)]
    types =     fmap varT names
    patterns =  fmap varP names
    variables = fmap varE names

  (fp, fv)     <- mkVars "f"
  (at, ap, av) <- mkVars2 "a"
  (bt, bp, bv) <- mkVars2 "b"

  let
    appA = appT (tupleT n) at
    appB = appT (tupleT n) bt

    aTupleT = foldl appT appA types
    bTupleT = foldl appT appB types

    typ = [t| Lens $aTupleT $bTupleT $at $bt |]

  sig <- sigD lensName typ

  let tupleExpr = tupE (bv:variables)
  body <- funD lensName [
      clause [fp, tupP (ap:patterns)] (normalB [e| (\ $bp -> $tupleExpr) <$> $fv $av |]) []
    ]

  pure [sig, body]


-- | Retrieve info about type
typeName :: Name -> Q Exp
typeName n = do
  info <- reify n
  case info of
    TyConI _ -> litE $ StringL "1"
    _ -> litE $ StringL "0"

listFields :: Name -> Q [String]
listFields dataName = do
  info <- reify dataName
  case info of
    TyConI (DataD _ _ _ _ cons _) -> processCons cons
    _ -> pure []
    where
      processCons :: [Con] -> Q [String]
      processCons cs = pure $ concat $ processCon <$> cs

      processCon c = case c of
        NormalC n ts -> fmap processT ts
          where
            processT (_, _) = nameBase n ++ "normal"
        RecC n ts -> fmap processT ts
          where
            processT (name, _, _) = nameBase n ++ " " ++ nameBase name
        _ -> []


listFields' :: Name -> ExpQ
listFields' n = do
  list <- listFields n
  listE (map stringE list)

