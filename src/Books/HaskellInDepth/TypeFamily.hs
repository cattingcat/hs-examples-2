{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Books.HaskellInDepth.TypeFamily (ushow, uprint) where

import Data.Char (showLitChar)
import Unsafe.Coerce (unsafeCoerce)

newtype UnescapingChar = UnescapingChar {charVal :: Char}

instance Show UnescapingChar where
  showsPrec _ (UnescapingChar '\'') = showString "'\\''"
  showsPrec _ (UnescapingChar c) = showChar '\'' . showLitChar' c . showChar '\''

  showList cs = showChar '"' . showLitString' (map charVal cs) . showChar '"'

showLitChar' :: Char -> ShowS
showLitChar' c s
  | c > '\DEL' = showChar c s
  | otherwise = showLitChar c s

showLitString' :: String -> ShowS
showLitString' [] s = s
showLitString' ('"' : cs) s = showString "\\\"" (showLitString' cs s)
showLitString' (c : cs) s = showLitChar' c (showLitString' cs s)

type ToUnescapingTF :: k -> k
type family ToUnescapingTF a where
  ToUnescapingTF Char = UnescapingChar
  ToUnescapingTF (t b) = (ToUnescapingTF t) (ToUnescapingTF b)
  ToUnescapingTF a = a

class ToUnescaping a where
  toUnescaping :: a -> ToUnescapingTF a

instance Show a => ToUnescaping a where
  toUnescaping = unsafeCoerce

type UnescapingShow a = (Show a, Show (ToUnescapingTF a))

ushow :: UnescapingShow a => a -> String
ushow = show . toUnescaping

uprint :: UnescapingShow a => a -> IO ()
uprint = putStrLn . ushow

-- uprint (Just 'ё')
-- print (Just 'ё')
