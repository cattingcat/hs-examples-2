{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Extensions.LiberalTypeSynonyms () where

type MyType = forall a . Show a => [a]
