module Gelatin.Core.Polyline where

import           Data.List (unzip5)
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector,Unbox)
import           Linear hiding (trace)

type PolylineData f =
  ( Vector (V2 Float)
  , Vector (f Float)
  , Vector (V2 Float)
  , Vector (V2 Float)
  , Vector (V2 Float)
  , Float
  )

expandPolyline :: Unbox (f Float)
               => Vector (V2 Float) -> Vector (f Float) -> Float -> Float
               -> Maybe (PolylineData f)
expandPolyline verts colors thickness feather
    | Just (v1,v2) <- (,) <$> (verts V.!? 0) <*> (verts V.!? 1)
    , Just c1  <- colors V.!? 0
    , Just (v3,v3n) <- (,) <$> (verts V.!? (V.length verts -1))
                           <*> (verts V.!? (V.length verts -2))
    , Just c3 <- colors V.!? (V.length verts -1) =
    let -- clamp the lower bound of our thickness to 1
        absthick = max thickness 1
        d = fromIntegral (ceiling $ absthick + 2.5 * feather :: Integer)
        lens = 0 `V.cons` V.zipWith distance verts (V.drop 1 verts)
        totalLen = V.foldl' (+) 0 lens
        totalEnd = totalLen + d
        seqfunc (total,ts) len = (total + len,ts V.++ V.singleton (total + len))
        seqLens  = snd $ V.foldl' seqfunc (0,mempty) lens
        isClosed = distance v1 v3 <= 0.00001
        -- if the polyline is closed return a miter with the last point
        startCap = ( V.fromList [cap,cap]
                   , V.fromList [c1,c1]
                   , uvs
                   , V.fromList [v2,v2]
                   , V.fromList [prev,prev]
                   )
            where (uvs,cap,prev) = if isClosed
                                   -- no cap
                                   then (V.fromList [V2 0 d, V2 0 (-d)],v1,v3n)
                                   -- cap
                                   else let c = d *^ signorm (v2 - v1)
                                        in ( V.fromList [V2 (-d) d, V2 (-d) (-d)]
                                           , v1 - c
                                           , v1 - 2*c)
        endCap = ( V.fromList [cap,cap]
                 , V.fromList [c3,c3]
                 , uvs
                 , V.fromList [next,next]
                 , V.fromList [v3n,v3n]
                 )
            where (uvs,cap,next) = if isClosed
                                   -- no cap
                                   then ( V.fromList [ V2 totalLen d
                                                     , V2 totalLen (-d)
                                                     ]
                                        , v3
                                        , v2
                                        )
                                   -- cap
                                   else let c = d *^ signorm (v3 - v3n)
                                        in (V.fromList [ V2 totalEnd d
                                                       , V2 totalEnd (-d)
                                                       ]
                                           , v3 + c
                                           , v3 + 2*c
                                           )
        vcs  = V.toList $ V.zip3 verts colors seqLens
        zs   = zipWith3 strp vcs (drop 1 vcs) (drop 2 vcs)
        -- Expand the line into a triangle strip
        strp :: Unbox (f Float)
             => (V2 Float, f Float, Float) -> (V2 Float, f Float, Float)
             -> (V2 Float, f Float, Float) -> (Vector (V2 Float)
                                              ,Vector (f Float)
                                              ,Vector (V2 Float)
                                              ,Vector (V2 Float)
                                              ,Vector (V2 Float)
                                              )
        strp (a,_,_) (b,bc,l) (c,_,_) =
          ( V.fromList [b,b]
          , V.fromList [bc,bc]
          , V.fromList [V2 l d,V2 l (-d)]
          , V.fromList [c,c]
          , V.fromList [a,a]
          )
        (vs,cs,us,ns,ps) = unzip5 $ startCap : zs ++ [endCap]
      in Just (V.concat vs, V.concat cs, V.concat us, V.concat ns, V.concat ps, totalLen)
    | otherwise = Nothing
