{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Gelatin.Picture.Shapes where

import           Control.Monad.State.Strict
import           Data.Vector.Unboxed        (Unbox)
import qualified Data.Vector.Unboxed        as V
import           Gelatin.Core
import           Gelatin.Picture.Internal
import           Linear                     hiding (rotate)
--------------------------------------------------------------------------------
-- Shapes (at the level of Vertices)
--------------------------------------------------------------------------------
curve :: (RealFloat a, Unbox a, Monad m)
      => V2 a -> V2 a -> V2 a
      -> VerticesT (V2 a) m ()
curve a b c =
  let vs  = subdivideAdaptive 100 0 $ bez3 a b c
  in Vertices $ modify (V.++ vs)

corner :: (RealFloat a, Unbox a, Monad m)
      => a -> a -> VerticesT (V2 a) m ()
corner xr yr =
  let vs = cleanSeqDupes $ V.concatMap (subdivideAdaptive 100 0) $ cornerBez3 xr yr
  in Vertices $ modify (V.++ vs)

arc :: (Unbox a, Epsilon a, RealFloat a, Monad m)
    => a -> a -> a -> a -> VerticesT (V2 a) m ()
arc w h start stop =
  let vs = cleanSeqDupes $ V.concatMap (subdivideAdaptive 100 0) $ arcBez3 w h start stop
  in Vertices $ modify (V.++ vs)

rectangle :: (Unbox a, Monad m)
          => V2 a -> V2 a -> VerticesT (V2 a) m ()
rectangle tl@(V2 tlx tly) br@(V2 brx bry) = do
  to tl
  to $ V2 brx tly
  to br
  to $ V2 tlx bry
