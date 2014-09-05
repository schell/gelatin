{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Gelatin.Compiling.IO where

import Gelatin.Rendering
import Gelatin.ShaderCommands
import Graphics.VinylGL
import Graphics.GLUtil hiding (Elem, setUniform)
import Graphics.Rendering.OpenGL hiding (position, color, VertexComponent)
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad.Free
import Control.Monad.Free.Church
import Data.Vinyl
import Data.Maybe
import Foreign

data RenderingDefaults = RenderingDefaults { rdefDepthFunc     :: Maybe ComparisonFunction
                                           , rdefShaderProgram :: Maybe ShaderProgram
                                           }

--------------------------------------------------------------------------------
-- Compiling/Running
--------------------------------------------------------------------------------
performDrawElementsCommand :: Free DrawElements () -> IO ()
performDrawElementsCommand (Pure ()) = return ()
performDrawElementsCommand (Free (DrawElements n mode next)) = do
    putStrLn "Drawing elements"
    GL.drawElements mode n UnsignedInt nullPtr
    performDrawElementsCommand next


performShaderCommand :: ShaderProgram -> Free ShaderOp () -> IO ()
performShaderCommand _ (Pure ()) = return ()
performShaderCommand s (Free (SetUniform u m next)) = do
    putStrLn "Setting a uniform."
    setUniforms s (u =: m)
    performShaderCommand s next
performShaderCommand s (Free (VertexComponent cmd next)) = do
    putStrLn "Buffering vertices."
    let vs = compileVertexComponents $ fromF cmd
    vbo <- bufferVertices vs
    enableVertices' s vbo
    performShaderCommand s next
    deleteVertices vbo
performShaderCommand s (Free (WithIndices ns cmd next)) = do
    putStrLn "Buffering indices."
    ebo <- bufferIndices ns
    bindBuffer ElementArrayBuffer $= Just ebo
    performDrawElementsCommand $ fromF cmd
    bindBuffer ElementArrayBuffer $= Nothing
    performShaderCommand s next

performRenderCommand :: RenderingDefaults -> Free Render () -> IO ()
performRenderCommand _ (Pure ()) = return ()
performRenderCommand rd (Free (UsingDepthFunc func r next)) = do
    putStrLn "Setting a depth func."
    depthFunc $= Just func
    performRenderCommand (rd{ rdefDepthFunc = Just func }) $ fromF r
    depthFunc $= (rdefDepthFunc rd)
    performRenderCommand rd next
performRenderCommand rd (Free (UsingShader s sc next)) = do
    putStrLn "Using a program."
    currentProgram $= (Just $ program s)
    performShaderCommand s $ fromF sc
    currentProgram $= (fmap program $ rdefShaderProgram rd)
    putStrLn ""
    performRenderCommand rd next

performRendering :: Rendering () -> IO ()
performRendering = performRenderCommand def . fromF
    where def = RenderingDefaults { rdefDepthFunc = Nothing
                                  , rdefShaderProgram = Nothing
                                  }
