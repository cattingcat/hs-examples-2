{-# LANGUAGE OverloadedStrings #-}

module Extensions.OverloadedStrings () where 
import Data.String

listOfStrs :: IsString s => [s]
listOfStrs = ["123"]
