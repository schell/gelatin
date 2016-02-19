{-# LANGUAGE DeriveGeneric #-}
module Gelatin.Core.Stroke where

import Gelatin.Core.Line
import Gelatin.Core.Color
import Data.Hashable
import GHC.Generics

data Stroke = Stroke { strokeColor    :: Color
                     , strokeColors   :: [Color]
                     , strokeWidth    :: Float
                     , strokeFeather  :: Float
                     , strokeLineCaps :: (LineCap,LineCap)
                     } deriving (Show, Eq, Generic)
instance Hashable Stroke

data StrokeAttr = StrokeNone
                | StrokeColor Color
                | StrokeColors [Color]
                | StrokeWidth Float
                | StrokeFeather Float
                | StrokeCaps (LineCap,LineCap)
                deriving (Show, Eq, Generic)

emptyStroke :: Stroke
emptyStroke = Stroke 0 [] 2 1 (LineCapRound,LineCapRound)

strokeAttr :: Maybe Stroke -> StrokeAttr -> Maybe Stroke
strokeAttr _ StrokeNone = Nothing
strokeAttr Nothing c = strokeAttr (Just emptyStroke) c
strokeAttr (Just s) (StrokeColor c) = Just $ s {strokeColor = c}
strokeAttr (Just s) (StrokeColors cs) = Just $ s {strokeColors = cs}
strokeAttr (Just s) (StrokeWidth w) = Just $ s {strokeWidth = w}
strokeAttr (Just s) (StrokeFeather t) = Just $ s {strokeFeather = t}
strokeAttr (Just s) (StrokeCaps cs) = Just $ s {strokeLineCaps = cs}
