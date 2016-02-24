module Gelatin.Core.Stroke where

import Gelatin.Core.Fill
import Gelatin.Core.Line
import Gelatin.Core.Color
import Data.Hashable
import Linear

data Stroke = Stroke { strokeFill     :: Fill 
                     , strokeWidth    :: Float
                     , strokeFeather  :: Float
                     , strokeLineCaps :: (LineCap,LineCap)
                     } deriving (Show)

data StrokeHash = StrokeHash Stroke [V2 Float]

instance Hashable StrokeHash where
    hashWithSalt s (StrokeHash (Stroke fill w f cs) vs) =
        s `hashWithSalt` FillHash fill vs `hashWithSalt` 
            w `hashWithSalt` f `hashWithSalt` cs 

data StrokeAttr = StrokeNone 
                | StrokeFill Fill
                | StrokeWidth Float
                | StrokeFeather Float
                | StrokeCaps (LineCap,LineCap)
                deriving (Show)

emptyStroke :: Stroke
emptyStroke = Stroke (solid 0) 2 1 (LineCapRound,LineCapRound)

strokeAttr :: Maybe Stroke -> StrokeAttr -> Maybe Stroke
strokeAttr _ StrokeNone = Nothing
strokeAttr Nothing c = strokeAttr (Just emptyStroke) c
strokeAttr (Just s) (StrokeFill f) = Just $ s {strokeFill = f}
strokeAttr (Just s) (StrokeWidth w) = Just $ s {strokeWidth = w}
strokeAttr (Just s) (StrokeFeather t) = Just $ s {strokeFeather = t}
strokeAttr (Just s) (StrokeCaps cs) = Just $ s {strokeLineCaps = cs}
