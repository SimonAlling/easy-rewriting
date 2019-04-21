{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Examples related to abstract syntax trees.
module Examples.AbstractSyntax where

import Prelude hiding ((>>))
import EasyRewriting.Rewrite (Rewrite, done, (>>))

-- | A very simple AST data type.
data Expr = Const Int | Expr :+: Expr
  deriving (Eq, Show)

-- | Evaluates an AST to the integer it represents.
eval :: Expr -> Int
eval (Const x) = x
eval (a :+: b) = eval a + eval b

{-|
Claims that ':+:' is associative. It's not, but it is with respect to 'eval'.

>>> check associativity
*** Incorrect!
Expression 1:
(Const 3 :+: Const 5) :+: Const 8
Expression 2:
Const 3 :+: (Const 5 :+: Const 8)
>>> checkOn eval associativity
Seems legit!
-}
associativity :: Rewrite Expr
associativity = do
  (Const 3 :+: Const 5) :+: Const 8
  Const 3 :+: (Const 5 :+: Const 8)
  Const 16
  done
