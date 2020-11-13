{-# LANGUAGE ScopedTypeVariables #-}

module Extensions.ScopedTypeVariables () where

foo :: forall a. a -> (a, a, a)
foo a =
  let makePair :: a -> (a, a) -- a is scoped
      makePair x = (x, x)

      (a1, a2) = makePair a
   in (a, a1, a2)
