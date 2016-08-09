{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Gelatin.GL.Common where

import           Gelatin
import           Gelatin.GL.Shader
import           Linear
--------------------------------------------------------------------------------
-- Transforming Pictures
--------------------------------------------------------------------------------
data PictureTransform = PictureTransform { ptfrmPosition :: V2 Float
                                         , ptfrmScale    :: V2 Float
                                         , ptfrmRotation :: Float
                                         , ptfrmAlpha    :: Float
                                         , ptfrmMultiply :: V4 Float
                                         } deriving (Show, Eq)

instance Monoid PictureTransform where
  mempty = PictureTransform 0 1 0 1 1
  mappend (PictureTransform ap as ar aa am)
          (PictureTransform bp bs br ba bm) =
    PictureTransform (ap + bp) (as * bs) (ar + br) (aa * ba) (am * bm)

instance HasAffine PictureTransform where
  move v   = mappend (PictureTransform v 1 0 1 1)
  scale v  = mappend (PictureTransform 0 v 0 1 1)
  rotate r = mappend (PictureTransform 0 1 r 1 1)

ptfrmAffine :: PictureTransform -> Transform
ptfrmAffine (PictureTransform p s r _ _) = Transform p s r

applyPicTfrmToBounds :: PictureTransform -> BBox -> BBox
applyPicTfrmToBounds = applyTfrmToBounds . ptfrmAffine
--------------------------------------------------------------------------------
-- Rendering Pictures
--------------------------------------------------------------------------------
type GLRenderer = (IO (), PictureTransform -> IO ())

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
--data SeqType = SeqStrip
--             | SeqFan
--             deriving (Show, Eq, Ord, Enum, Generic)
--
--instance Hashable SeqType
--
--data GPrims a = GPTris (Vector (a,a,a))
--              | GPBezs (Vector (a,a,a))
--              | GPLine Stroke (Vector a)
--              | GPTriSeq SeqType a a a (Vector a)
--              deriving (Generic)
--
--instance (Hashable a, Unbox a) => Hashable (GPrims a)
--
--compilePrims :: (Unbox (f Float), Additive f)
--             => Geometry (V2 Float, f Float) ()
--             -> B.Vector (GPrims (V2 Float, f Float))
--compilePrims = collect . fromF
--
--collect :: (Unbox (f Float), Additive f)
--        => Free (Geom (V2 Float, f Float)) ()
--        -> B.Vector (GPrims (V2 Float, f Float))
--collect (Pure ()) = {-# SCC "collect.Pure" #-}B.empty
--collect (Free (GNone n)) = {-# SCC "collect.GNone" #-}collect n
--collect (Free (GTriangles ts n)) =
--  {-# SCC "collect.GTriangle" #-}GPTris ts `B.cons` collect n
--collect (Free (GBezs bs n)) =
--  {-# SCC "collect.GBez" #-}GPBezs bs `B.cons` collect n
--collect (Free (GTriangleStrip a b c ds n)) =
--  {-# SCC "collect.GTriangleStrip" #-}GPTriSeq SeqStrip a b c ds `B.cons` collect n
--collect (Free (GTriangleFan a b c ds n)) =
--  {-# SCC "collect.GTriangleFan" #-}GPTriSeq SeqFan a b c ds `B.cons` collect n
--collect (Free (GLine l n)) =
--  let lns = B.map (GPLine emptyStroke) $ linesToPolylines l
--  in {-# SCC "collect.GLine" #-}lns B.++ collect n
----------------------------------------------------------------------------------
---- Compiled picture types
----------------------------------------------------------------------------------
--
--instance Hashable SpecialOp
--
--data CompiledPicture a =
--    CompiledTex SpecialOp (B.Vector (GPrims (V2 Float, V2 Float))) a
--  | CompiledColor SpecialOp (B.Vector (GPrims (V2 Float, V4 Float)))
--  | CompiledLine SpecialOp Stroke (B.Vector (GPrims (V2 Float, V4 Float)))
--  deriving (Generic)
--
--instance Hashable a => Hashable (CompiledPicture a)
--
