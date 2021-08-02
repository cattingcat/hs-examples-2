{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    # nativeBuildInputs is usually what you want -- tools you need to run
    nativeBuildInputs = [ pkgs.haskellPackages.cabal-install pkgs.haskell.compiler.ghc901 ];
}