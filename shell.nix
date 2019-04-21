{ pkgs ? import <nixpkgs> {} }:

(import ./default.nix { inherit pkgs; }).env.overrideAttrs (old: {
  nativeBuildInputs = old.nativeBuildInputs ++ [
    pkgs.cabal-install
  ];
})
