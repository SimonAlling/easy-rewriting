{ pkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:

pkgs.haskell.packages.${compiler}.callCabal2nix "easy-rewriting" ./. {}