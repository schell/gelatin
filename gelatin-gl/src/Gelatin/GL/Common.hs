{-# LANGUAGE DeriveGeneric #-}
module Gelatin.GL.Common where

import Gelatin.Core
import Gelatin.Picture
import Gelatin.GL.Shader
import Data.Renderable
import Data.Hashable
import GHC.Generics
import Linear
import Control.Monad.Free
import Control.Monad.Free.Church

type GLRenderer = Renderer IO Transform

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

data GPrims a = GPTris [(a,a,a)]
              | GPBezs [(a,a,a)]
              | GPLine Stroke (LineSegment a) [LineSegment a]
              | GPTriSeq SeqType a a a [a]
              deriving (Generic)

instance Hashable a => Hashable (GPrims a)

compilePrims :: Geometry a () -> [GPrims a]
compilePrims = collect Nothing . fromF

collect :: Maybe (GPrims a) -> Free (Geom a) () -> [GPrims a]
collect Nothing (Pure ()) = []
collect Nothing (Free (GNone n)) = collect Nothing n
collect Nothing (Free (GTriangle a b c n)) =
  collect (Just $ GPTris [(a,b,c)]) n
collect Nothing (Free (GBez a b c n)) =
  collect (Just $ GPBezs [(a,b,c)]) n
collect Nothing (Free (GTriangleStrip a b c ds n)) =
  GPTriSeq SeqStrip a b c ds : collect Nothing n
collect Nothing (Free (GTriangleFan a b c ds n)) =
  GPTriSeq SeqFan a b c ds : collect Nothing n
collect Nothing (Free (GLine l n)) =
  let segs = map (uncurry $ GPLine emptyStroke) $ lineToSegments l
  in segs ++ collect Nothing n
collect (Just (GPTris ts)) (Free (GTriangle a b c n)) =
  collect (Just (GPTris $ ts ++ [(a,b,c)])) n
collect (Just (GPBezs ts)) (Free (GBez a b c n)) =
  collect (Just (GPBezs $ ts ++ [(a,b,c)])) n
collect (Just p) g =
  p : collect Nothing g
--------------------------------------------------------------------------------
-- Compiled picture types
--------------------------------------------------------------------------------
data SpecialOp = SOpNone
               | SOpStencilMask
               deriving (Show, Eq, Generic)

instance Hashable SpecialOp

data CompiledPicture a = CompiledTex SpecialOp [GPrims (V2 Float, V2 Float)] a
                       | CompiledColor SpecialOp [GPrims (V2 Float, V4 Float)]
                       | CompiledLine SpecialOp Stroke [GPrims (V2 Float, V4 Float)]
                       deriving (Generic)

instance Hashable a => Hashable (CompiledPicture a)

