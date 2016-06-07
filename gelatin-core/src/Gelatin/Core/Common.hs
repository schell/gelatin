{-# LANGUAGE DeriveGeneric #-}
module Gelatin.Core.Common where

import GHC.Generics
import Data.Hashable

newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Generic)

instance Hashable Uid where
  hashWithSalt s = hashWithSalt s . unUid
