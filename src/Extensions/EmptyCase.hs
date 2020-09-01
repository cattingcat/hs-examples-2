{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Extensions.EmptyCase () where

import Data.Void (Void)

data MyData = A | B

-- compiller warning
foo :: MyData -> a
foo a = case a of {}

bar :: Void -> a
bar v = case v of {}

 