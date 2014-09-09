{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Gelatin.ShaderCommands where

import Linear
import Graphics.VinylGL
import Graphics.GLUtil hiding (Elem, setUniform)
import Graphics.Rendering.OpenGL hiding (position, color, VertexComponent)
import Control.Monad.Free
import Control.Monad.Free.Church
import Data.Vinyl
import Data.Vinyl.Universe
import Data.Vinyl.Reflect
import Foreign.Storable

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data DrawElements next = DrawElements GLint PrimitiveMode next
data DrawArrays next = DrawArrays GLint PrimitiveMode next

data ShaderOp next where
    SetUniform :: ( HasFieldNames (PlainFieldRec '[n ::: t])
                  , HasFieldGLTypes (PlainFieldRec '[n ::: t])
                  , SetUniformFields (PlainFieldRec '[n ::: t])
                  ) => SField (n:::t) -> t -> next -> ShaderOp next
    SetVertices :: ( Storable (PlainFieldRec rs)
                   , BufferSource (v (PlainFieldRec rs))
                   , HasFieldDims (PlainFieldRec rs)
                   , HasFieldNames (PlainFieldRec rs)
                   , HasFieldSizes (PlainFieldRec rs)
                   , HasFieldGLTypes (PlainFieldRec rs)
                   ) => v (PlainFieldRec rs) -> next -> ShaderOp next
    WithIndices :: [Word32] -> DrawElementsCommand () -> next -> ShaderOp next

type DrawElementsCommand = F DrawElements
type DrawArraysCommand = F DrawArrays
type ShaderCommand = F ShaderOp

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Functor DrawElements where
    fmap f (DrawElements n mode next) = DrawElements n mode $ f next

instance Functor DrawArrays where
    fmap f (DrawArrays n mode next) = DrawArrays n mode $ f next

instance Functor ShaderOp where
    fmap f (SetUniform u d next) = SetUniform u d $ f next
    fmap f (SetVertices vs next) = SetVertices vs $ f next
    fmap f (WithIndices ns cmd next) = WithIndices ns cmd $ f next
--------------------------------------------------------------------------------
-- User API
--------------------------------------------------------------------------------
drawElements :: GLint -> PrimitiveMode -> DrawElementsCommand ()
drawElements n mode = liftF $ DrawElements n mode ()

drawArrays :: GLint -> PrimitiveMode -> DrawArraysCommand ()
drawArrays n mode = liftF $ DrawArrays n mode ()

setUniform :: ( HasFieldNames (PlainFieldRec '[n ::: t])
              , HasFieldGLTypes (PlainFieldRec '[n ::: t])
              , SetUniformFields (PlainFieldRec '[n ::: t])
              ) => SField (n:::t) -> t -> ShaderCommand ()
setUniform u d = liftF $ SetUniform u d ()

setVertices :: ( Storable (PlainFieldRec rs)
               , MonadFree ShaderOp m
               , BufferSource (v (PlainFieldRec rs))
               , HasFieldDims (PlainFieldRec rs)
               , HasFieldNames (PlainFieldRec rs)
               , HasFieldSizes (PlainFieldRec rs)
               , HasFieldGLTypes (PlainFieldRec rs)
               ) => v (PlainFieldRec rs) -> m ()
setVertices vs = liftF $ SetVertices vs ()

withIndices :: [Word32] -> DrawElementsCommand () -> ShaderCommand ()
withIndices ns cmd = liftF $ WithIndices ns cmd ()
--------------------------------------------------------------------------------
-- Matrix Helpers
--------------------------------------------------------------------------------
rotateX :: (Epsilon a, Floating a) => a -> Quaternion a
rotateX = axisAngle (V3 1 0 0)

rotateY :: (Epsilon a, Floating a) => a -> Quaternion a
rotateY = axisAngle (V3 0 1 0)

rotateZ :: (Epsilon a, Floating a) => a -> Quaternion a
rotateZ = axisAngle (V3 0 0 1)

transform :: Real a => V3 a -> V3 a -> Quaternion a -> M44 a
transform p (V3 sx sy sz) q = t !*! s !*! q'
    where t  = mkTransformationMat eye3 p
          q' = mkTransformation q $ V3 0 0 0
          s  = V4 (V4 sx 0 0  0)
                  (V4 0 sy 0  0)
                  (V4 0  0 sz 0)
                  (V4 0  0 0  1)
