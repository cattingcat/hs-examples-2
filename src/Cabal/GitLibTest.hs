{-# LANGUAGE QuasiQuotes#-}

module Cabal.GitLibTest (countStr) where

import Language.Befunge.TH

-- | See cabal.project
countStr :: String
countStr =
  [befunge|
          |>987v>.v |
          |v456<  : |
          |>321 ^ _@|]

