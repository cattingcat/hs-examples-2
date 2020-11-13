module Main (main) where

import System.Directory
import qualified Tests.DocTest as DocTest
import qualified Tests.TastyTest as TastyTest

main :: IO ()
main = do
  dir <- getCurrentDirectory
  putStrLn $ "Current dir: " ++ dir
  DocTest.main
  TastyTest.main
  putStrLn "Test finish"
