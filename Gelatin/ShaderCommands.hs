{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Gelatin.ShaderCommands where

import Linear
import Graphics.VinylGL
import Graphics.GLUtil hiding (Elem, setUniform)
import Graphics.Rendering.OpenGL hiding (position, color, VertexComponent)
import Control.Monad.Free
import Control.Monad.Free.Church
import Data.Maybe
import Data.Vinyl
import Data.Vinyl.Universe
import Data.Vinyl.Reflect
import Foreign.Storable

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data DrawElements next = DrawElements GLint PrimitiveMode next
data DrawArrays next = DrawArrays GLint PrimitiveMode next

data VertexComponent next where
    VertexComponent :: ( Storable t
                       , HasFieldDims (PlainFieldRec '[n ::: t])
                       , HasFieldNames (PlainFieldRec '[n ::: t])
                       , HasFieldSizes (PlainFieldRec '[n ::: t])
                       , HasFieldGLTypes (PlainFieldRec '[n ::: t])
                       ) => SField (n:::t) -> [t] -> next -> VertexComponent next

data ShaderOp next where
    SetUniform :: ( HasFieldNames (PlainFieldRec '[n ::: t])
                  , HasFieldGLTypes (PlainFieldRec '[n ::: t])
                  , SetUniformFields (PlainFieldRec '[n ::: t])
                  ) => SField (n:::t) -> t -> next -> ShaderOp next
    SetVertices :: VertexComponentCommand () -> next -> ShaderOp next
    WithIndices :: [Word32] -> DrawElementsCommand () -> next -> ShaderOp next

type DrawElementsCommand = F DrawElements
type DrawArraysCommand = F DrawArrays
type VertexComponentCommand = F VertexComponent
type ShaderCommand = F ShaderOp
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Functor DrawElements where
    fmap f (DrawElements n mode next) = DrawElements n mode $ f next

instance Functor DrawArrays where
    fmap f (DrawArrays n mode next) = DrawArrays n mode $ f next

instance Functor VertexComponent where
    fmap f (VertexComponent u d next) = VertexComponent u d $ f next

instance Functor ShaderOp where
    fmap f (SetUniform u d next) = SetUniform u d $ f next
    fmap f (SetVertices cmd next) = SetVertices cmd $ f next
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

addVertexComponent :: ( Storable t
                      , HasFieldDims (PlainFieldRec '[n ::: t])
                      , HasFieldNames (PlainFieldRec '[n ::: t])
                      , HasFieldSizes (PlainFieldRec '[n ::: t])
                      , HasFieldGLTypes (PlainFieldRec '[n ::: t])
                      ) => SField (n:::t) -> [t] -> VertexComponentCommand ()
addVertexComponent u d = liftF $ VertexComponent u d ()

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

--compileVertexComponent :: ( Storable (PlainFieldRec rs)
--                          , BufferSource (v (PlainFieldRec rs))
--                          ) => Maybe (v (PlainFieldRec rs))
--                            -> Free VertexComponent ()
--                            -> Maybe (v (PlainFieldRec rs))
compileVertexComponent vs (Pure ()) = vs
compileVertexComponent Nothing (Free (VertexComponent v vs next)) =
    compileVertexComponent (Just vs') next
        where vs' = map (v =:) vs

compileVertexComponent (Just vs') (Free (VertexComponent v vs next)) =
    compileVertexComponent (Just vs') next
        where vs'' = zipWith (<+>) vs' (map (v =:) vs)

compileVertexComponents cmd = fromJust $ compileVertexComponent Nothing $ fromF cmd
