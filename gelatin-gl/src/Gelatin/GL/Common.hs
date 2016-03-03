{-# LANGUAGE DeriveGeneric #-}
module Gelatin.GL.Common where

import           Gelatin.Core
import           Gelatin.Picture
import           Gelatin.GL.Shader
import           Data.Renderable
import           Data.Hashable
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector, Unbox)
import           GHC.Generics
import           Linear
import           Control.Monad.Free
import           Control.Monad.Free.Church

type GLRenderer = Renderer IO PictureTransform

data Context = Context { ctxFramebufferSize :: IO (Int,Int)
                       , ctxWindowSize :: IO (Int,Int)
                       , ctxScreenDpi :: IO Int
                       }

data Rez = Rez { rezShader  :: SumShader
               , rezContext :: Context
               }
--------------------------------------------------------------------------------
-- Primitives and attributes
--------------------------------------------------------------------------------
data SeqType = SeqStrip
             | SeqFan
             deriving (Show, Eq, Ord, Enum, Generic)

instance Hashable SeqType

data GPrims a = GPTris (Vector (a,a,a))
              | GPBezs (Vector (a,a,a))
              | GPLine Stroke (Vector a)
              | GPTriSeq SeqType a a a (Vector a)
              deriving (Generic)

instance (Hashable a, Unbox a) => Hashable (GPrims a)

compilePrims :: (Unbox (f Float), Additive f)
             => Geometry (V2 Float, f Float) ()
             -> B.Vector (GPrims (V2 Float, f Float))
compilePrims = collect . fromF

collect :: (Unbox (f Float), Additive f)
        => Free (Geom (V2 Float, f Float)) ()
        -> B.Vector (GPrims (V2 Float, f Float))
collect (Pure ()) = {-# SCC "collect.Pure" #-}B.empty
collect (Free (GNone n)) = {-# SCC "collect.GNone" #-}collect n
collect (Free (GTriangles ts n)) =
  {-# SCC "collect.GTriangle" #-}GPTris ts `B.cons` collect n
collect (Free (GBezs bs n)) =
  {-# SCC "collect.GBez" #-}GPBezs bs `B.cons` collect n
collect (Free (GTriangleStrip a b c ds n)) =
  {-# SCC "collect.GTriangleStrip" #-}GPTriSeq SeqStrip a b c ds `B.cons` collect n
collect (Free (GTriangleFan a b c ds n)) =
  {-# SCC "collect.GTriangleFan" #-}GPTriSeq SeqFan a b c ds `B.cons` collect n
collect (Free (GLine l n)) =
  let lns = B.map (GPLine emptyStroke) $ linesToPolylines l
  in {-# SCC "collect.GLine" #-}lns B.++ collect n
--------------------------------------------------------------------------------
-- Compiled picture types
--------------------------------------------------------------------------------
data SpecialOp = SOpNone
               | SOpStencilMask
               deriving (Show, Eq, Generic)

instance Hashable SpecialOp

data CompiledPicture a =
    CompiledTex SpecialOp (B.Vector (GPrims (V2 Float, V2 Float))) a
  | CompiledColor SpecialOp (B.Vector (GPrims (V2 Float, V4 Float)))
  | CompiledLine SpecialOp Stroke (B.Vector (GPrims (V2 Float, V4 Float)))
  deriving (Generic)

instance Hashable a => Hashable (CompiledPicture a)

