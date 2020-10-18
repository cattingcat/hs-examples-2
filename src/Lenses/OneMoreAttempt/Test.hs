module Lenses.OneMoreAttempt.Test () where 


l :: [Int]
l = [1,2,3,4,5,6]

-- | Doctest integration
-- >>> foo
-- "123456"
foo :: String
foo = concatMap show l


