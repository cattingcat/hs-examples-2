module Tests.DocTest (main) where

import Test.DocTest

main = doctest ["-isrc", "src/Books/HaskellInDepth/DocTests.hs"]
