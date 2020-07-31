{-# LANGUAGE TemplateHaskell #-}
module Extensions.TH.TemplateHaskell (
add1,
add2,
mkPat,
bar
) where

import Language.Haskell.TH

add1 :: Int -> Q Exp
add1 x = [| x + 1 |]

-- | Typed template haskell 
add2 :: Int -> Q (TExp Int)
add2 x = [|| x + 2 ||]

-- pattern
mkPat :: Q Pat
mkPat = [p| (x, y) |]

bar :: Q Exp
bar = [| \ $(mkPat) -> x : w |]