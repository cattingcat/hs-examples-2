# app-template

## Setup
``` 
    ghcup list
    ghcup install ghc 8.10.2
    ghcup install cabal
```

## Working
```
    cabal update
    
    cabal build

    cabal repl
      :m +Module.Name

    cabal run

    cabal install  [optional package name]
```

## Formatting
```
  ./tool/format.sh
```