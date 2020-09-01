module Main (main) where
import Extensions.Unboxed

main :: IO ()
main = do
  let r = fmap (\(a, b) -> s a b) (zip [1..10000] [1..10000])
  print (sum r)
