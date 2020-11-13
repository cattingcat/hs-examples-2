{-# LANGUAGE TypeFamilies #-}

module Books.HaskellInDepth.DataFamily () where

import Control.Conditional (if')
import Data.Word

data family XList a

newtype instance XList () = XListUnit Word -- List of units isomorphic to number

data instance XList Bool = XBits Word32 Word -- Bit set

class XListable a where
  xempty :: XList a
  xcons :: a -> XList a -> XList a
  xheadMay :: XList a -> Maybe a

instance XListable () where
  xempty = XListUnit 0
  xcons _ (XListUnit n) = XListUnit (n + 1)
  xheadMay (XListUnit 0) = Nothing
  xheadMay _ = Just ()

instance XListable Bool where
  xempty = XBits 0 0
  xcons b (XBits bits n) = XBits (bits * 2 + if' b 1 0) (n + 1)
  xheadMay (XBits _ 0) = Nothing
  xheadMay (XBits bits _) = Just (odd bits)
