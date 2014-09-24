{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Gelatin.ShaderCommands where

import Graphics.VinylGL
import Graphics.GLUtil hiding (Elem, setUniform)
import Graphics.Rendering.OpenGL hiding (position, color, VertexComponent, drawElements)
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Applicative
import Data.Vinyl
import Data.Vinyl.Reflect
import Data.Vinyl.TyFun
import Foreign.Storable

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data ShaderUniform u = ShaderUniform { uniformName :: String
                                     , uniformData :: u
                                     }

data VertexComponent v a = VertexComponent { vertexName       :: String
                                           , vertexData       :: [v]
                                           , vertexHandling   :: IntegerHandling
                                           , vertexDescriptor :: VertexArrayDescriptor a
                                           }

data VertexBufferOp next where
    AddComponent :: Storable v => VertexComponent v a -> next -> VertexBufferOp next

data DrawElements next = DrawElements GLint PrimitiveMode next


data ShaderOp next where
    SetUniform :: AsUniform u => ShaderUniform u -> next -> ShaderOp next
    WithVertexBuffer :: VertexBufferCommand () -> ShaderCommand () -> next -> ShaderOp next
    WithVertices :: ( Storable (PlainFieldRec rs)
                    , BufferSource (v (PlainFieldRec rs))
                    , HasFieldDims (PlainFieldRec rs)
                    , HasFieldNames (PlainFieldRec rs)
                    , HasFieldSizes (PlainFieldRec rs)
                    , HasFieldGLTypes (PlainFieldRec rs)
                    ) => v (PlainFieldRec rs) -> ShaderCommand () -> next -> ShaderOp next
    WithIndices :: Integral i => [i] -> DrawElementsCommand () -> next -> ShaderOp next
    DrawArrays :: Integral i => PrimitiveMode -> i -> next -> ShaderOp next

type VertexBufferCommand = F VertexBufferOp
type DrawElementsCommand = F DrawElements
type ShaderCommand = F ShaderOp
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance AsUniform u => AsUniform (ShaderUniform u) where
   asUniform su loc = asUniform (uniformData su) loc

instance Functor VertexBufferOp where
    fmap f (AddComponent c next) = AddComponent c $ f next

instance Functor DrawElements where
    fmap f (DrawElements n mode next) = DrawElements n mode $ f next

instance Functor ShaderOp where
    fmap f (SetUniform u next) = SetUniform u $ f next
    fmap f (WithVertexBuffer vb cmd next) = WithVertexBuffer vb cmd $ f next
    fmap f (WithVertices vs cmd next) = WithVertices vs cmd $ f next
    fmap f (WithIndices ns cmd next) = WithIndices ns cmd $ f next
    fmap f (DrawArrays n mode next) = DrawArrays n mode $ f next
--------------------------------------------------------------------------------
-- User API
--------------------------------------------------------------------------------
addComponent :: Storable v => VertexComponent v a -> VertexBufferCommand ()
addComponent c = liftF $ AddComponent c ()

drawElements :: GLint -> PrimitiveMode -> DrawElementsCommand ()
drawElements n mode = liftF $ DrawElements n mode ()

setUniform :: AsUniform u => ShaderUniform u -> ShaderCommand ()
setUniform u = liftF $ SetUniform u ()

withVertexBuffer :: VertexBufferCommand () -> ShaderCommand () -> ShaderCommand ()
withVertexBuffer vcmd scmd = liftF $ WithVertexBuffer vcmd scmd ()

withVertices :: ( Storable (PlainFieldRec rs)
             , BufferSource (v (PlainFieldRec rs))
             , HasFieldDims (PlainFieldRec rs)
             , HasFieldNames (PlainFieldRec rs)
             , HasFieldSizes (PlainFieldRec rs)
             , HasFieldGLTypes (PlainFieldRec rs)
             ) => v (PlainFieldRec rs) -> ShaderCommand () -> ShaderCommand ()
withVertices vs cmd = liftF $ WithVertices vs cmd ()

withIndices :: Integral i => [i] -> DrawElementsCommand () -> ShaderCommand ()
withIndices ns cmd = liftF $ WithIndices ns cmd ()

drawIndexedTriangles :: Integral i => [i] -> i -> ShaderCommand ()
drawIndexedTriangles ns i = withIndices ns $ drawElements (fromIntegral $ i*3) Triangles

drawArrays :: Integral i => PrimitiveMode -> i -> ShaderCommand ()
drawArrays mode n = liftF $ DrawArrays mode n ()
--------------------------------------------------------------------------------
-- Making vertices from components
--------------------------------------------------------------------------------
comp :: Applicative f => sing k1 -> [el Data.Vinyl.TyFun.$ k1] -> [Rec el f '[k1]]
comp a as = map (a =:) as

(.+) :: [Rec el f as] -> [Rec el f bs] -> [Rec el f (as ++ bs)]
(.+) = zipWith (<+>)
