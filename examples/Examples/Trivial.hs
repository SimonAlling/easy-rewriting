{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Trivial examples.
module Examples.Trivial where

import Prelude hiding ((>>))
import EasyRewriting.Rewrite (Rewrite, done, (>>))

-- | The empty rewrite chain (always valid).
empty :: Rewrite Int
empty = done

-- | A singleton rewrite chain (always valid).
singleton :: Rewrite Int
singleton = do
  5
  done
