{-|
This module contains functions for checking correctness of 'Rewrite' chains.

>>> check [ 5 + 7, 12 ]
Seems legit!
>>> check [ 1 + 2, 12 ]
*** Incorrect!
Expression 1:
3
Expression 2:
12
-}
module EasyRewriting.Check
  ( Index
  , Judgment(..)
  , check
  , checkOn
  , checkAt
  , equalityOfFunctions
  ) where

import EasyRewriting.Rewrite (Rewrite)

-- | A position in a rewrite chain. The first expression in a chain has index 1.
type Index = Int

-- | The result of checking a rewrite chain. The 'SeemsLegit' constructor is so named because this library doesn't do formal proofs.
data Judgment a = SeemsLegit | Incorrect Index a a
  deriving (Eq)

instance Show a => Show (Judgment a) where
  show SeemsLegit = "Seems legit!"
  show (Incorrect i a b) = init . unlines $
    [ "*** Incorrect!"
    , label (i - 1)
    , show a
    , label i
    , show b
    ]
    where label n = "Expression " ++ show n ++ ":"

{-|
Checks if all values in a list are equal.

>>> check [ 5 + 7, 12 ]
Seems legit!
-}
check :: Eq a => Rewrite a -> Judgment a
check = checkOn id

{-|
Checks if all values in a list are equal under application of a given function.

>>> checkOn length [ "Kiren", "Musen" ]
Seems legit!
-}
checkOn :: Eq b => (a -> b) -> Rewrite a -> Judgment b
checkOn = checkIndex 2 -- first possibly erroneous expression
  where
    checkIndex :: Eq b => Index -> (a -> b) -> Rewrite a -> Judgment b
    checkIndex _ _ [ ] = SeemsLegit
    checkIndex _ _ [_] = SeemsLegit
    checkIndex i f (x : xs@(y:_)) =
      if fx == fy then checkIndex (i+1) f xs else Incorrect i fx fy
      where (fx, fy) = (f x, f y)

{-|
Checks if all functions in a list are equal under application to a given value.

>>> checkAt (Just 42) [ fmap id, id ]
Seems legit!

If the functions are binary:

>>> checkAt (negate, [1, 5, -8]) $ map uncurry [ map, fmap ]
Seems legit!
-}
checkAt :: Eq b => a -> Rewrite (a -> b) -> Judgment b
checkAt = checkOn . flip ($)

{-|
Can be used with <https://hackage.haskell.org/package/QuickCheck QuickCheck> to test equality of functions. Note that testing is never perfect.

>>> quickCheck $ equalityOfFunctions [ (*1), id ]
Seems legit!
-}
equalityOfFunctions :: Eq b => Rewrite (a -> b) -> a -> Bool
equalityOfFunctions fs x = checkAt x fs == SeemsLegit
