{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Extensions.UndecidableInstances () where

class D c where {}
class Conv a b where {}

-- loop 
instance (Conv a b, D b) => D a where {}

