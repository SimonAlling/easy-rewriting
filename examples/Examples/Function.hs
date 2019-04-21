{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Examples related to functions.
module Examples.Function where

import Prelude hiding ((>>))
import EasyRewriting.Rewrite (Rewrite, done, (>>))

-- | Claims that 'id' ∘ 'id' ∘ 'id' = 'id' ∘ 'id' = 'id'.
identity :: Rewrite (Int -> Int)
identity = do
  id . id . id
  id . id
  id
  done

-- | Claims that 'almostId' is the same function as 'id'.
incorrectIdentity :: Rewrite (Int -> Int)
incorrectIdentity = do
  id . id . id
  almostId
  id
  done

-- | Almost the identity function for 'Int'; returns @0@ if given @1@.
almostId :: Int -> Int
almostId x
  | x == 1 = 0
  | otherwise = x

-- | Demonstrates a typical conversion from pointful to pointfree composition.
pointfree :: (b -> c) -> (a -> b) -> Rewrite (a -> c)
pointfree f g = do
  \x -> f (g x)
  \x -> (f . g) x
  f . g
  done