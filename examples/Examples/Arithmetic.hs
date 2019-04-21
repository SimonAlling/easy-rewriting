{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-|
Examples related to arithmetic.
-}
module Examples.Arithmetic where

import Prelude hiding ((>>))
import EasyRewriting.Rewrite (Rewrite, done, (>>))

{-|
Claims that 3 + 2 + 7 = 5 + 7 = 12.
-}
twelve :: Rewrite Int
twelve = do
  3 + 2 + 7
  5     + 7
  12
  done

{-|
Claims that 30 − 15 + 5 = 30 − 20 = 10.
-}
incorrectSimplification :: Rewrite Int
incorrectSimplification = do
  30 - 15 + 5
  30 - 20
  10
  done
