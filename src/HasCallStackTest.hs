{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HasCallStackTest () where

import GHC.Stack.Types(HasCallStack)

lol :: HasCallStack => Int
lol = error "sdfsd"

foo :: HasCallStack => String
foo = "asd" <> "" <> show lol

class MyClass a where 
  bar :: a -> String
  
instance MyClass Int where 
  bar :: HasCallStack => Int -> String
  bar = undefined
  
instance MyClass String where 
  bar s = foo
  
