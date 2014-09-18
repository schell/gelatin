{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Gelatin.ShaderCommands where

import Graphics.VinylGL
import Graphics.GLUtil hiding (Elem, setUniform)
import Graphics.Rendering.OpenGL hiding (position, color, VertexComponent, drawElements)
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Applicative
import Data.Vinyl
import Data.Vinyl.Universe
import Data.Vinyl.Reflect
import Data.Vinyl.TyFun
import Foreign.Storable

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data DrawElements next = DrawElements GLint PrimitiveMode next

data ShaderOp next where
    SetUniform :: ( HasFieldNames (PlainFieldRec '[n ::: t])
                  , HasFieldGLTypes (PlainFieldRec '[n ::: t])
                  , SetUniformFields (PlainFieldRec '[n ::: t])
                  ) => SField (n:::t) -> t -> next -> ShaderOp next
    WithVertices :: ( Storable (PlainFieldRec rs)
                 , BufferSource (v (PlainFieldRec rs))
                 , HasFieldDims (PlainFieldRec rs)
                 , HasFieldNames (PlainFieldRec rs)
                 , HasFieldSizes (PlainFieldRec rs)
                 , HasFieldGLTypes (PlainFieldRec rs)
                 ) => v (PlainFieldRec rs) -> ShaderCommand () -> next -> ShaderOp next
    WithIndices :: [Word32] -> DrawElementsCommand () -> next -> ShaderOp next
    DrawArrays :: PrimitiveMode -> GLint -> next -> ShaderOp next

type DrawElementsCommand = F DrawElements
type ShaderCommand = F ShaderOp
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Functor DrawElements where
    fmap f (DrawElements n mode next) = DrawElements n mode $ f next

instance Functor ShaderOp where
    fmap f (SetUniform u d next) = SetUniform u d $ f next
    fmap f (WithVertices vs cmd next) = WithVertices vs cmd $ f next
    fmap f (WithIndices ns cmd next) = WithIndices ns cmd $ f next
    fmap f (DrawArrays n mode next) = DrawArrays n mode $ f next
--------------------------------------------------------------------------------
-- User API
--------------------------------------------------------------------------------
drawElements :: GLint -> PrimitiveMode -> DrawElementsCommand ()
drawElements n mode = liftF $ DrawElements n mode ()

setUniform :: ( HasFieldNames (PlainFieldRec '[n ::: t])
              , HasFieldGLTypes (PlainFieldRec '[n ::: t])
              , SetUniformFields (PlainFieldRec '[n ::: t])
              ) => SField (n:::t) -> t -> ShaderCommand ()
setUniform u d = liftF $ SetUniform u d ()

withVertices :: ( Storable (PlainFieldRec rs)
             , BufferSource (v (PlainFieldRec rs))
             , HasFieldDims (PlainFieldRec rs)
             , HasFieldNames (PlainFieldRec rs)
             , HasFieldSizes (PlainFieldRec rs)
             , HasFieldGLTypes (PlainFieldRec rs)
             ) => v (PlainFieldRec rs) -> ShaderCommand () -> ShaderCommand ()
withVertices vs cmd = liftF $ WithVertices vs cmd ()

withIndices :: [Word32] -> DrawElementsCommand () -> ShaderCommand ()
withIndices ns cmd = liftF $ WithIndices ns cmd ()

drawIndexedTriangles :: [Word32] -> GLint -> ShaderCommand ()
drawIndexedTriangles ns i = withIndices ns $ drawElements (i*3) Triangles

drawArrays :: PrimitiveMode -> GLint -> ShaderCommand ()
drawArrays mode n = liftF $ DrawArrays mode n ()
--------------------------------------------------------------------------------
-- Making vertices from components
--------------------------------------------------------------------------------
comp :: Applicative f => sing k1 -> [el Data.Vinyl.TyFun.$ k1] -> [Rec el f '[k1]]
comp a as = map (a =:) as

(.+) :: [Rec el f as] -> [Rec el f bs] -> [Rec el f (as ++ bs)]
(.+) = zipWith (<+>)
