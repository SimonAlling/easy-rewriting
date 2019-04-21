{-|
A DSL that aids in rewriting expressions and checking correctness. Intended for equational reasoning, semi-formal proofs etc.


= Basic example

@
{\-\# LANGUAGE RebindableSyntax #-\}

import Prelude hiding ((>>))
import EasyRewriting

mySimplification :: 'Rewrite' Int
mySimplification = do
  3 + 5 + 8
  8     + 8
  16
  'done'
@

Save the code above as @Test.hs@ and run @ghci Test.hs@ to open it in GHCi.
You can then check that @mySimplification@ is correct:

>>> check mySimplification
Seems legit!

Note that the @RebindableSyntax@ extension is necessary for the code above to work, and so is @import Prelude hiding ((>>))@.
However, @'Rewrite' a@ simply means @[a]@, so you don't have to use @RebindableSyntax@ if you don't care about the specialized @do@ notation:

@
import EasyRewriting

mySimplification :: 'Rewrite' Int
mySimplification =
  [ 3 + 5 + 8
  , 8     + 8
  , 16
  ]
@


= When are two expressions "equal"?

If your expressions are more complicated than integers, you may not want to simply check for equality.
(Maybe your data type isn't even in 'Eq'!)

@
data Expr = Const Int | Expr :+: Expr
  deriving (Eq, Show)

eval :: Expr -> Int
eval (Const x) = x
eval (a :+: b) = eval a + eval b

associativity :: 'Rewrite' Expr
associativity = do
  (Const 3 :+: Const 5) :+: Const 8
  Const 3 :+: (Const 5 :+: Const 8)
  Const 16
  'done'
@

In this case, a plain equality 'check' will fail:

>>> check associativity
*** Incorrect!
Expression 1:
(Const 3 :+: Const 5) :+: Const 8
Expression 2:
Const 3 :+: (Const 5 :+: Const 8)

With 'checkOn', you can instead check for equality under application of a given function:

>>> checkOn eval associativity
Seems legit!


= Equality of functions

Functions can't be compared for equality, but you can use 'checkAt' to check if all functions in a list return the same value when applied to a given argument.
Then you can use <https://hackage.haskell.org/package/QuickCheck QuickCheck> to generate such arguments.
Conveniently, 'equalityOfFunctions' helps with this:

>>> let fs = [ (*1), id ]
>>> checkAt 5 fs
Seems legit!
>>> quickCheck (equalityOfFunctions fs)
+++ OK, passed 100 tests.


= Submodules

This module re-exports everything from the submodules below.
If, for example, you only want to /create/ rewrite chains, not check them, you can import only the @Rewrite@ submodule:

@
{\-\# LANGUAGE RebindableSyntax #-\}

import Prelude hiding ((>>))
import EasyRewriting.Rewrite

mySimplification :: 'Rewrite' a
mySimplification = do
  ...
@
-}

module EasyRewriting
  ( module EasyRewriting.Check
  , module EasyRewriting.Rewrite
  ) where

import EasyRewriting.Check
import EasyRewriting.Rewrite
