{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Hashable.Vector where

import Data.Hashable
import Data.Vector.Unboxed
import qualified Data.Vector as V

instance (Hashable a, Unbox a) => Hashable (Vector a) where
  hashWithSalt s = Data.Vector.Unboxed.foldl' hashWithSalt s

instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt s = V.foldl' hashWithSalt s
