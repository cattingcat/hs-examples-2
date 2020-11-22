{-# LANGUAGE BlockArguments #-}

module Extensions.BlockArguments where

foo :: IO () -> IO () -> IO ()
foo a b = a >> b

tst :: IO ()
tst = 
  foo
    do putStrLn "fst"
    do putStrLn "snd"