{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Gelatin.Picture.Shapes where

import           Gelatin.Core
import           Gelatin.Picture.Internal
import           Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import           Control.Monad.State.Strict
import           Linear hiding (rotate)
--------------------------------------------------------------------------------
-- Shapes (at the level of Vertices)
--------------------------------------------------------------------------------
curve :: (RealFloat a, Unbox a, Enum a)
      => V2 a -> V2 a -> V2 a
      -> State (Vector (V2 a)) ()
curve a b c =
  let vs  = subdivideAdaptive 100 0 $ bez3 a b c
  in modify (V.++ vs)

corner :: (RealFloat a, Unbox a, Enum a)
      => a -> a -> State (Vector (V2 a)) ()
corner xr yr =
  let vs = cleanSeqDupes $ V.concatMap (subdivideAdaptive 100 0) $ cornerBez3 xr yr
  in modify (V.++ vs)

arc :: (Unbox a, RealFloat a)
    => a -> a -> a -> a -> State (Vector (V2 a)) ()
arc w h start stop =
  let vs = cleanSeqDupes $ V.concatMap (subdivideAdaptive 100 0) $ arcBez3 w h start stop
  in modify (V.++ vs)

rectangle :: (RealFloat a, Unbox a)
          => V2 a -> V2 a -> State (Vector (V2 a)) ()
rectangle tl@(V2 tlx tly) br@(V2 brx bry) = do
  to tl
  to $ V2 brx tly
  to $ br
  to $ V2 tlx bry
