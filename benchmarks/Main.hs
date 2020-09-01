{-# LANGUAGE MagicHash, UnboxedTuples, UnboxedSums #-}
module Main (main) where
import Books.HaskellInDepth.BenchFuncs
import Extensions.Unboxed
import Criterion.Main


main :: IO ()
main = defaultMain
  [ primeBenches
  , unboxedBenches
  ]


primeBenches = bgroup "PrimeBenches"
  [ isPrimeBench
  , isPrime'Bench
  ]

primeNum = 16183
isPrimeBench = bench "isPrime" $ whnf isPrime primeNum
isPrime'Bench = bench "isPrime'" $ whnf isPrime' primeNum



unboxedBenches = bgroup "Unboxed benches"
  [ bench "Boxed" $ nf (s 11) 221
  , bench "UnBoxed" $ nf (s' 11) 221
  ]