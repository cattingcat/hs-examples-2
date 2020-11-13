{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Extensions.UndecidableInstances () where

class D c

class Conv a b

-- loop
instance (Conv a b, D b) => D a
