# easy-rewriting

This library aids in rewriting Haskell expressions correctly. It is intended for equational reasoning, semi-formal proofs etc, and designed to be easy to use.

The [`examples`](examples/) directory contains examples of how it can be used, as does the [documentation](https://hackage.haskell.org/package/easy-rewriting).


## Install

Add `easy-rewriting` to the `build-depends` list in your `.cabal` file, or run `cabal install easy-rewriting` or `stack install easy-rewriting`.


## Use

Please refer to the [documentation at Hackage](https://hackage.haskell.org/package/easy-rewriting).


## Contribute

You need GHC and Cabal.
If you're on NixOS, you can run `nix-shell` to start a shell where you can run `cabal`.

### Build

Build the package:

    cabal build

Generate documentation:

    cabal haddock

On NixOS, you can run `nix-build` to build everything.
If you get an error like `ghc: can't find a package database ...`, you might need to delete `.ghc.environment.x86_64-linux-8.4.4` or similar.
It worked for me at least.

### Test

Run the test suite:

    cabal test

Since the library is designed to be used quite interactively, you probably want to try it out in GHCi as well:

    cabal new-build
    ghci examples/Examples/Monad.hs
    > import EasyRewriting
    > checkAt (Just (Just True)) defineJoinUsingBind
    Seems legit!

(`cabal new-build` creates a GHC environment file (e.g. `.ghc.environment.x86_64-linux-8.4.4`) which makes it possible to import `EasyRewriting` in GHCi without installing `easy-rewriting` globally.
If it doesn't work, you can try `cabal install` instead of `cabal new-build`.)

You can create and check rewrite chains on the fly using GHCi's multiline syntax (`:{ ... :}`):

    cabal new-build
    ghci
    > :set -XRebindableSyntax -XNoImplicitPrelude
    > import Prelude hiding ((>>))
    > import EasyRewriting
    > :{
    | check $ do
    |   8 + 5
    |   5 + 8
    |   done
    | :}
    Seems legit!
