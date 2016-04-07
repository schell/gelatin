{-# LANGUAGE DeriveGeneric #-}
module Gelatin.Core.Stroke where

import Gelatin.Core.Line
import Data.Hashable
import Data.Maybe (fromMaybe)
import GHC.Generics

data Stroke = Stroke { strokeWidth    :: Float
                     , strokeFeather  :: Float
                     , strokeLineCaps :: (LineCap,LineCap)
                     } deriving (Show, Generic)

instance Hashable Stroke

data StrokeAttr = StrokeNone
                | StrokeWidth Float
                | StrokeFeather Float
                | StrokeCaps (LineCap,LineCap)
                deriving (Show, Generic)

instance Hashable StrokeAttr

emptyStroke :: Stroke
emptyStroke = Stroke 2 1 (LineCapRound,LineCapRound)

strokeAttr :: Maybe Stroke -> StrokeAttr -> Maybe Stroke
strokeAttr _ StrokeNone = Nothing
strokeAttr Nothing c = strokeAttr (Just emptyStroke) c
strokeAttr (Just s) (StrokeWidth w) = Just $ s {strokeWidth = w}
strokeAttr (Just s) (StrokeFeather t) = Just $ s {strokeFeather = t}
strokeAttr (Just s) (StrokeCaps cs) = Just $ s {strokeLineCaps = cs}

strokeWith :: [StrokeAttr] -> Stroke
strokeWith atts = fromMaybe emptyStroke $ foldl strokeAttr Nothing atts
