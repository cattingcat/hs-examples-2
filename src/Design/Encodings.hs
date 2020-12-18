{-# LANGUAGE RankNTypes #-}
module Design.Encodings (
    hsList2List
) where 

data List a = Nil | Cons a (List a)

newtype Fix f = Fix {unFix :: f (Fix f) }
data ListF a f = NilF | ConsF a f

type List' a = Fix (ListF a)

newtype ListC a = ListC {runList :: forall r . r -> (a -> r -> r) -> r}

hsList2List :: [a] -> List a
hsList2List [] = Nil
hsList2List (x:xs) = Cons x (hsList2List xs)

hsList2List' :: [a] -> List' a
hsList2List' [] = Fix NilF
hsList2List' (x:xs) = Fix $ ConsF x (hsList2List' xs)

hsList2ListC :: [a] -> ListC a
hsList2ListC [] = ListC $ \n _ -> n
hsList2ListC (x:xs) = ListC $ \n c -> c x (runList (hsList2ListC xs) n c)

listC2HsList :: ListC a -> [a]
listC2HsList (ListC fn) = fn [] (:)




tst1 :: [a] -> [a]
tst1 = listC2HsList . hsList2ListC