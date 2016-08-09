{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Gelatin.Core.Common where

import GHC.Generics
import Data.Hashable

newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Generic, Num, Enum)

instance Hashable Uid where
  hashWithSalt s = hashWithSalt s . unUid
