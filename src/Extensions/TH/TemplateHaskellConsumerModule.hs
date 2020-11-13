{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Extensions.TH.TemplateHaskellConsumerModule
  ( Person (..),
  )
where

import Data.Functor.Identity
import Extensions.TH.THLenses
import Extensions.TH.TemplateHaskell
import Language.Haskell.TH

data Person = MkPerson {_name :: String, _age :: Int}

two :: Int
two = $(add1 1)

foo :: (Char, String) -> String
foo $(mkPat) = x : y -- z -- impossible to use z instead of y because of typecheck

-- Embed template haskell definitions
createMap2Func

-- use custom quasi quoter
customBrackets :: Int
customBrackets = [myQuoter| + |]

--tryToUseInPatternPlace :: (Int, Int) -> Int
--tryToUseInPatternPlace [myQuoter| *  |] = 6

$(fstLensTH "kek")
fstLensTH "puk"

$(fstOfNLensTH "fstOf3" 3)

tst1 = kek (\x -> Identity $ x + 55) (1, 2)

tst2 = puk (\x -> Identity $ x + 55) (1, 2)

tst3 = fstOf3 (\x -> Identity $ x + 55) (1, 2, 3)

tst4 = $(typeName ''Bool)

tst5 = $(typeName ''Eq)

tst6 = $(typeName 'True)

tst7 = $(stringE . show =<< reify ''Bool)

tst8 = $(listFields' ''Person)

tst9 = [showQQ|tst8|]
