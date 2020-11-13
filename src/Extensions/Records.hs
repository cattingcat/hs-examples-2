{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Extensions.Records where

import GHC.Records (HasField (..))

data MyRecord = MkMyRecord
  { a :: Int,
    s :: String
  }
  deriving stock (Show)

foo :: MyRecord -> Int
foo MkMyRecord {..} = a

bar :: MyRecord -> MyRecord
bar r = r {a = 55}

baz :: HasField "s" r String => r -> String
baz r = reverse $ getField @"s" r

tst1 = baz $ MkMyRecord 55 "qwe"
