{-# LANGUAGE BangPatterns #-}

module Extensions.BangPatterns () where

data MyData a = MkMyData a

-- | ~ means "lazy match"
fooNonStrict :: MyData a -> String
fooNonStrict ~(MkMyData a) = "kek"

tst1 = fooNonStrict undefined

-- | Match ctor but don't calculate a
fooDefault :: MyData a -> String
fooDefault (MkMyData a) = "kek"

tst2 = fooDefault undefined

tst3 = fooDefault (MkMyData undefined)

-- | match ctor and DO calcualte a because of bang (!)
fooStrict :: MyData a -> String
fooStrict (MkMyData !a) = "kek"

tst4 = fooStrict (MkMyData undefined)

tst5 = fooStrict (MkMyData 4)

data T a = C a -- lazy by-default but strict with StrictData
--data T' a = C' ~a - works only with StrictData

data T'' a = C'' !a -- strict
