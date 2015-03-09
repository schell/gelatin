{-# LANGUAGE GADTs #-}
module Gelatin.Core.ShaderCommands where

import Graphics.GLUtil hiding (Elem, setUniform)
import Graphics.Rendering.OpenGL hiding (position, color, VertexComponent, drawElements)
import Control.Monad.Free
import Control.Monad.Free.Church
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
    ShaderM :: IO () -> next -> ShaderOp next
    SetUniform :: AsUniform u => ShaderUniform u -> next -> ShaderOp next
    WithVertices :: VertexBufferCommand () -> ShaderCommand () -> next -> ShaderOp next
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
    fmap f (ShaderM m next) = ShaderM m $ f next
    fmap f (SetUniform u next) = SetUniform u $ f next
    fmap f (WithVertices vb cmd next) = WithVertices vb cmd $ f next
    fmap f (WithIndices ns cmd next) = WithIndices ns cmd $ f next
    fmap f (DrawArrays n mode next) = DrawArrays n mode $ f next
--------------------------------------------------------------------------------
-- User API
--------------------------------------------------------------------------------
shaderM :: IO () -> ShaderCommand ()
shaderM io = liftF $ ShaderM io ()

addComponent :: Storable v => VertexComponent v a -> VertexBufferCommand ()
addComponent c = liftF $ AddComponent c ()

drawElements :: GLint -> PrimitiveMode -> DrawElementsCommand ()
drawElements n mode = liftF $ DrawElements n mode ()

setUniform :: AsUniform u => ShaderUniform u -> ShaderCommand ()
setUniform u = liftF $ SetUniform u ()

withVertices :: VertexBufferCommand () -> ShaderCommand () -> ShaderCommand ()
withVertices vcmd scmd = liftF $ WithVertices vcmd scmd ()

withIndices :: Integral i => [i] -> DrawElementsCommand () -> ShaderCommand ()
withIndices ns cmd = liftF $ WithIndices ns cmd ()

drawIndexedTriangles :: Integral i => [i] -> i -> ShaderCommand ()
drawIndexedTriangles ns i = withIndices ns $ drawElements (fromIntegral $ i*3) Triangles

drawArrays :: Integral i => PrimitiveMode -> i -> ShaderCommand ()
drawArrays mode n = liftF $ DrawArrays mode n ()
