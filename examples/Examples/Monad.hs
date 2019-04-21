{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Examples related to monads.
module Examples.Monad where

import Prelude hiding ((>>))
import EasyRewriting.Rewrite (Rewrite, done, (>>))
import Control.Monad (join)

-- | Demonstrates how '>>=' can be defined using 'join'.
defineBindUsingJoin :: Monad m => Rewrite (m a -> (a -> m b) -> m b)
defineBindUsingJoin = do
  (>>=)
  \ma f -> ma >>= f
  -- f already returns an m b; fmap "adds an extra m" that join then removes:
  \ma f -> join (fmap f ma)
  \ma f -> join (flip fmap ma f) -- so we can get rid of f
  \ma   -> join . flip fmap ma
  (join .) . flip fmap
  done

-- | Demonstrates how 'join' can be defined using '>>='.
defineJoinUsingBind :: Monad m => Rewrite (m (m a) -> m a)
defineJoinUsingBind = do
  join
  \mma -> join mma
  -- We have an m (m a) and want an m a; let's just take the one inside mma:
  \mma -> mma >>= id
  (>>= id)
  done
