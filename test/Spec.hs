import qualified DocTest as D
import qualified TastyTest as T
import System.Directory

main :: IO ()
main = do
  dir <- getCurrentDirectory
  putStrLn $ "Current dir: " ++ dir
  D.main
  T.main
  putStrLn "Test finish"
