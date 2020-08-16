{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module IxMon () where

import Prelude hiding ((>>=), (>>))




data FileStatus = Opened | Closed

data FileDesriptor (s :: FileStatus) = Fd

open :: FileDesriptor 'Closed -> FileDesriptor 'Opened
open = undefined

close :: FileDesriptor 'Opened -> FileDesriptor 'Closed
close = undefined

read :: FileDesriptor 'Opened -> String
read = undefined

class IxMonad m where
  ipure :: a -> m s s a
  ibind :: m i j a -> (a -> m j k b) -> m i k b

newtype Fs (i :: FileStatus) (j :: FileStatus) a = MkFs a

instance IxMonad Fs where
  ipure a = MkFs a
  ibind (MkFs a) f = let (MkFs b) = f a in MkFs b

(>>=) :: Fs i j a -> (a -> Fs j k b) -> Fs i k b
(>>=) = ibind

(>>) :: Fs i j a -> Fs j k b -> Fs i k b
(>>) a b = a `ibind` const b


open' :: String -> Fs 'Closed 'Opened ()
open' = undefined

close' :: Fs 'Opened 'Closed ()
close' = undefined

read' :: Fs 'Opened 'Opened String
read' = ipure "qwe"

--testprogram :: String -> Fs Closed Closed String
--testprogram s = (open'          s) `ibind` (\_ ->
--                 read')            `ibind` (\r ->
--                 close'            `ibind` (\_ ->
--                 read'             `ibind` (\r2 ->
--                 ipure r2)))

tstprog = do
  open' ""
  s <- read'
  close'
--  s2 <- read'
  ipure s