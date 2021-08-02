module Design.Selective () where
import Data.Functor
import Control.Selective

-- class Applicative f => Selective f where
--   select :: f (Either a b) -> f (a -> b) -> f b

selectAp :: Applicative f => f (Either a b) -> f (a -> b) -> f b
selectAp e f = (transformFunc <$> f) <*> e
  where
    transformFunc :: (a -> b) -> (Either a b -> b)
    transformFunc f' (Left a) = f' a
    transformFunc _ (Right b) = b

-- Selective instance could be implemented via applicative. 
--   But it has to evaluate effect of @f (a -> b)@
tst1 = select (Just (Right 1)) undefined
tst1' = selectAp (Just (Right 1)) undefined

tst2 = select (pure (Right 1)) (print "boom" *> pure id)
tst2' = selectAp (pure (Right 1)) (print "boom" *> pure id)

branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
branch x l r = select elimLeft r
  where 
    -- tst :: f (Either a (Either b c))
    tst = fmap (fmap Left) x

    -- tst' :: f (a -> Either b c)
    tst' = fmap (\f -> Right . f) l

    -- elimLeft :: f (Either b c)
    elimLeft = select tst tst'
