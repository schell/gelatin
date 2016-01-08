{-# LANGUAGE DeriveGeneric #-}
module Gelatin.Core.Line where

import GHC.Generics
import Data.Hashable

data LineCap = LineCapNone
             | LineCapButt
             | LineCapSquare
             | LineCapRound
             | LineCapTriOut
             | LineCapTriIn
             deriving (Show, Eq, Enum, Generic)

instance Hashable LineCap
