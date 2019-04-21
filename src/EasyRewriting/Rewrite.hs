{-|
This module defines a little DSL for rewrite chains.

A rewrite chain is just a list of expressions intended to be equal to each other.
This module only provides convenient syntactic sugar for equational reasoning:

@
{\-\# LANGUAGE RebindableSyntax #-\}

import Prelude hiding ((>>))
import EasyRewriting.Rewrite

mySimplification :: 'Rewrite' Int
mySimplification = do
  3 + 5 + 8
  8     + 8
  16
  'done'
@
-}
module EasyRewriting.Rewrite
  ( Rewrite
  , done
  , (>>)
  ) where

import Prelude hiding ((>>))

-- | A chain of expressions intended to be equal to each other.
type Rewrite = []

-- | Defining @(>>) = (:)@ makes for nice @do@ notation rewrite chains together with @RebindableSyntax@:
(>>) :: a -> [a] -> [a]
(>>) = (:)

-- | Terminates rewrite chains nicely.
done :: Rewrite a
done = []