{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import Books.HaskellInDepth.BenchFuncs
import Criterion.Main
import Extensions.Unboxed

main :: IO ()
main =
  defaultMain
    [ primeBenches,
      unboxedBenches
    ]

primeBenches =
  bgroup
    "PrimeBenches"
    [ isPrimeBench,
      isPrime'Bench
    ]

primeNum = 16183

isPrimeBench = bench "isPrime" $ whnf isPrime primeNum

isPrime'Bench = bench "isPrime'" $ whnf isPrime' primeNum

unboxedBenches =
  bgroup
    "Unboxed benches"
    [ bench "Boxed" $ nf (s 11) 221,
      bench "UnBoxed" $ nf (s' 11) 221
    ]
