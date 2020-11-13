{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Extensions.LiberalTypeSynonyms () where

type MyType = forall a. Show a => [a]
