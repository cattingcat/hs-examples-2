#!/bin/sh

cabal update
cabal install ormolu 
ormolu --mode inplace $(find . -wholename './*.hs')
echo "Done!"