{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module Gelatin.GL.Shader (
  -- * Compiling and loading shaders
    Simple2DShader
  , compileOGLShader
  , compileOGLProgram
  , loadSourcePaths
  , compileSources
  , compileProgram
  , loadSimple2DShader

    --loadSumShader,
    --loadGLShader,
    ---- * GLShader types
    --GLShaderProgram,
    --GLShader,
    --GLShaderDef,
      ---- * GLShader prims
    --PrimType(..),
    ---- * Uniforms
    --Simple2DUniform(..),
    ---- * Updating uniforms for specific kinds of rendering
    --updateUniformsForTris,
    --updateUniformsForBezs,
    --updateUniformsForLines,
    --updateUniformsForMask,
    --applyAlpha,
    --applyMult,
    ---- * Free form uniform updates
    --updateUniform,
    --updateUniforms,
    ---- * Attributes
    ---- $layout
    --Simple2DAttrib(..),
    --locToGLuint,
    ---- * Enabling attribs for specific kinds of rendering
    --enableAttribsForTris,
    --enableAttribsForBezs,
    --enableAttribsForLines,
    --enableAttribsForMask,
    ---- * Enabling and disabling any attribs
    --onlyEnableAttribs,
    ---- * GLShader compilation
    --compileShader,
    --compileProgram,
) where

import           Control.Exception      (assert)
import           Control.Monad
import           Control.Monad.Except   (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString.Char8  as B
import qualified Data.Foldable          as F
import           Data.Proxy             (Proxy (..))
import qualified Data.Vector.Generic    as G
import qualified Data.Vector.Storable   as S
import           Data.Vector.Unboxed    (Unbox, Vector)
import qualified Data.Vector.Unboxed    as V
import           Foreign.C.String
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.TypeLits           (KnownNat, KnownSymbol, natVal)
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Prelude                hiding (init)
import           Prelude                as P

import           Gelatin
import           Gelatin.Shaders

import           Gelatin.GL.TH


type Simple2DShader = GLuint
--------------------------------------------------------------------------------
-- Updating shader uniforms
--------------------------------------------------------------------------------
---- | Updates uniforms for rendering triangles.
--updateUniformsForTris :: GLShader -> M44 Float -> M44 Float -> Bool -> Float
--                      -> V4 Float -> Maybe (V4 Float) -> IO ()
--updateUniformsForTris sh pj mv hasUV a m mr =
--  updateUniforms (uniformsForTris pj mv hasUV a m mr) sh
--{-# INLINE updateUniformsForTris #-}
--
---- | Updates uniforms for rendering loop-blinn beziers.
--updateUniformsForBezs :: GLShader -> M44 Float -> M44 Float -> Bool -> Float
--                      -> V4 Float -> Maybe (V4 Float) -> IO ()
--updateUniformsForBezs sh pj mv hasUV a m mr =
--  updateUniforms (uniformsForBezs pj mv hasUV a m mr) sh
--{-# INLINE updateUniformsForBezs #-}
--
---- | Updates uniforms for rendering projected polylines.
--updateUniformsForLines :: GLShader -> M44 Float -> M44 Float -> Bool -> Float
--                       -> V4 Float -> Maybe (V4 Float) -> Float -> Float -> Float
--                       -> (LineCap,LineCap) -> IO ()
--updateUniformsForLines sh pj mv hasUV a m mr thickness feather sumlength caps =
--  let us = uniformsForLines pj mv hasUV a m mr thickness feather sumlength caps
--  in updateUniforms us sh
--{-# INLINE updateUniformsForLines #-}
--
---- | Updates uniforms for rendering alpha masking.
--updateUniformsForMask :: GLShader -> M44 Float -> M44 Float -> Float -> V4 Float
--                      -> GLuint -> GLuint -> IO ()
--updateUniformsForMask sh pj mv a m main mask =
--  updateUniforms (uniformsForMask pj mv a m main mask) sh
--{-# INLINE updateUniformsForMask #-}
--
--updateUniforms :: [Simple2DUniform] -> GLShader -> IO ()
--updateUniforms us s = mapM_ (`updateUniform` s) us
--{-# INLINE updateUniforms #-}
--
--updateUniform :: Simple2DUniform -> GLShader -> IO ()
--updateUniform u s = withUniform (simple2DUniformIdentifier u) s $ \p loc -> do
--  glUseProgram p
--  uniformUpdateFunc u loc
--{-# INLINE updateUniform #-}
--
--uniformUpdateFunc :: Simple2DUniform -> GLint -> IO ()
--uniformUpdateFunc (UniformPrimType p) u =
--  glUniform1i u $ fromIntegral $ fromEnum p
--uniformUpdateFunc (UniformProjection m44) u =
--  with m44 $ glUniformMatrix4fv u 1 GL_TRUE . castPtr
--uniformUpdateFunc (UniformModelView m44) u =
--  with m44 $ glUniformMatrix4fv u 1 GL_TRUE . castPtr
--uniformUpdateFunc (UniformThickness t) u = glUniform1f u t
--uniformUpdateFunc (UniformFeather f) u = glUniform1f u f
--uniformUpdateFunc (UniformSumLength l) u = glUniform1f u l
--uniformUpdateFunc (UniformLineCaps (capx, capy)) u =
--  let [x,y] = P.map (fromIntegral . fromEnum) [capx,capy] in glUniform2f u x y
--uniformUpdateFunc (UniformHasUV has) u = glUniform1i u $ if has then 1 else 0
--uniformUpdateFunc (UniformSampler s) u = glUniform1i u $ fromIntegral s
--uniformUpdateFunc (UniformMainTex t) u = glUniform1i u $ fromIntegral t
--uniformUpdateFunc (UniformMaskTex t) u = glUniform1i u $ fromIntegral t
--uniformUpdateFunc (UniformAlpha a) u = glUniform1f u $ realToFrac a
--uniformUpdateFunc (UniformMult v) u =
--  let (V4 r g b a) = realToFrac <$> v in glUniform4f u r g b a
--uniformUpdateFunc (UniformShouldReplaceColor s) u =
--  glUniform1i u $ if s then 1 else 0
--uniformUpdateFunc (UniformReplaceColor c) u =
--  let (V4 r g b a) = realToFrac <$> c in glUniform4f u r g b a
--{-# INLINE uniformUpdateFunc #-}
--
--withUniform :: String -> GLShader -> (GLuint -> GLint -> IO ()) -> IO ()
--withUniform name (Shader p ls) f =
--    case lookup name ls of
--        Nothing  -> do P.putStrLn $ "could not find uniform " ++ name
--                       exitFailure
--        Just loc -> f p loc
--{-# INLINE withUniform #-}
--------------------------------------------------------------------------------
-- $layout
-- Attributes layout locations are unique and global.
--------------------------------------------------------------------------------
--locToGLuint :: Simple2DAttrib -> GLuint
--locToGLuint = fromIntegral . fromEnum
--
---- | Enables the provided attributes and disables all others.
--onlyEnableAttribs :: [Simple2DAttrib] -> IO ()
--onlyEnableAttribs atts = do
--   mapM_ (glDisableVertexAttribArray . locToGLuint) allAttribs
--   mapM_ (glEnableVertexAttribArray . locToGLuint) atts

--enableAttribsForTris :: Bool -> IO ()
--enableAttribsForTris True  = onlyEnableAttribs [PositionLoc,UVLoc]
--enableAttribsForTris False = onlyEnableAttribs [PositionLoc,ColorLoc]
--
--enableAttribsForBezs :: Bool -> IO ()
--enableAttribsForBezs True  = onlyEnableAttribs [PositionLoc,UVLoc,BezLoc]
--enableAttribsForBezs False = onlyEnableAttribs [PositionLoc,ColorLoc,BezLoc]
--
--enableAttribsForLines :: Bool -> IO ()
--enableAttribsForLines True =
--  onlyEnableAttribs [PositionLoc,UVLoc,BezUVLoc,NextLoc,PrevLoc]
--enableAttribsForLines False =
--  onlyEnableAttribs [PositionLoc,ColorLoc,BezUVLoc,NextLoc,PrevLoc]
--
--enableAttribsForMask :: IO ()
--enableAttribsForMask = onlyEnableAttribs [PositionLoc,UVLoc]
--------------------------------------------------------------------------------
-- IsShaderType instances
--------------------------------------------------------------------------------
instance IsShaderType VertexShader GLenum where
  getShaderType _ = GL_VERTEX_SHADER

instance IsShaderType FragmentShader GLenum where
  getShaderType _ = GL_FRAGMENT_SHADER
--------------------------------------------------------------------------------
-- Uniform marshaling functions
--------------------------------------------------------------------------------
$(genUniform [t|Bool|] [| \loc bool ->
   glUniform1i loc $ if bool then 1 else 0 |])

$(genUniform [t|Int|] [| \loc enum ->
   glUniform1i loc $ fromIntegral $ fromEnum enum |])

$(genUniform [t|PrimType|] [| \loc enum ->
   glUniform1i loc $ fromIntegral $ fromEnum enum |])

$(genUniform [t|Float|] [| \loc float ->
   glUniform1f loc $ realToFrac float |])

$(genUniform [t|V2 Float|] [| \loc v ->
   let V2 x y = fmap realToFrac v
   in glUniform2f loc x y |])

$(genUniform [t|V3 Float|] [| \loc v ->
   let V3 x y z = fmap realToFrac v
   in glUniform3f loc x y z|])

$(genUniform [t|V4 Float|] [| \loc v ->
  let (V4 r g b a) = realToFrac <$> v
  in glUniform4f loc r g b a |])

$(genUniform [t|M44 Float|] [| \loc val ->
  with val $ glUniformMatrix4fv loc 1 GL_TRUE . castPtr |])

$(genUniform [t|(Int,Int)|] [| \loc (a, b) ->
   let [x,y] = P.map fromIntegral [a, b]
   in glUniform2i loc x y |])

$(genUniform [t|(LineCap,LineCap)|] [| \loc (a, b) ->
   let [x,y] = P.map (fromIntegral . fromEnum) [a, b]
   in glUniform2f loc x y |])

$(genUniform [t|V2 Int|] [| \loc v ->
   let V2 x y = fmap fromIntegral v
   in glUniform2i loc x y |])
--------------------------------------------------------------------------------
-- Attribute buffering and toggling
--------------------------------------------------------------------------------
convertVec
  :: (Unbox (f Float), Foldable f) => Vector (f Float) -> S.Vector GLfloat
convertVec =
  S.convert . V.map realToFrac . V.concatMap (V.fromList . F.toList)

instance
  ( KnownNat loc, KnownSymbol name
  , Foldable f
  , Unbox (f Float), Storable (f Float)
  ) => HasGenFunc (AttributeBuffering (Attribute name (f Float) loc)) where

  type GenFunc (AttributeBuffering (Attribute name (f Float) loc)) =
    GLint -> GLuint -> Vector (f Float) -> IO ()
  genFunction _ n buf as = do
    let loc = fromIntegral $ natVal (Proxy :: Proxy loc)
        asize = V.length as * sizeOf (V.head as)
    glBindBuffer GL_ARRAY_BUFFER buf
    S.unsafeWith (convertVec as) $ \ptr ->
      glBufferData GL_ARRAY_BUFFER (fromIntegral asize) (castPtr ptr) GL_STATIC_DRAW
    glEnableVertexAttribArray loc
    glVertexAttribPointer loc n GL_FLOAT GL_FALSE 0 nullPtr
    err <- glGetError
    when (err /= 0) $ do
      print err
      assert False $ return ()

instance (KnownNat loc, KnownSymbol name)
  => HasGenFunc (AttributeToggling (Attribute name val loc)) where
  type GenFunc (AttributeToggling (Attribute name val loc)) = (IO (), IO ())
  genFunction _ =
    let aloc  = fromIntegral $ natVal (Proxy :: Proxy loc)
    in (glEnableVertexAttribArray aloc, glDisableVertexAttribArray aloc)
--------------------------------------------------------------------------------
-- OpenGL shader only stuff
--------------------------------------------------------------------------------
compileOGLShader :: (MonadIO m, MonadError String m)
                 => ByteString
                 -- ^ The shader source
                 -> GLenum
                 -- ^ The shader type (vertex, frag, etc)
                 -> m GLuint
                 -- ^ Either an error message or the generated shader handle.
compileOGLShader src shType = do
  shader <- liftIO $ glCreateShader shType
  if shader == 0
    then throwError "Could not create shader"
    else do
      success <- liftIO $ do
        withCString (B.unpack src) $ \ptr ->
          with ptr $ \ptrptr -> glShaderSource shader 1 ptrptr nullPtr

        glCompileShader shader
        with (0 :: GLint) $ \ptr -> do
          glGetShaderiv shader GL_COMPILE_STATUS ptr
          peek ptr

      if success == GL_FALSE
        then do
          err <- liftIO $ do
            infoLog <- with (0 :: GLint) $ \ptr -> do
                glGetShaderiv shader GL_INFO_LOG_LENGTH ptr
                logsize <- peek ptr
                allocaArray (fromIntegral logsize) $ \logptr -> do
                    glGetShaderInfoLog shader logsize nullPtr logptr
                    peekArray (fromIntegral logsize) logptr

            return $ P.unlines [ "Could not compile shader:"
                               , B.unpack src
                               , P.map (toEnum . fromEnum) infoLog
                               ]
          throwError err
        else return shader

compileOGLProgram :: (MonadIO m, MonadError String m)
                  => [(String, Integer)] -> [GLuint] -> m GLuint
compileOGLProgram attribs shaders = do
    (program, success) <- liftIO $ do
      program <- glCreateProgram
      forM_ shaders (glAttachShader program)
      forM_ attribs $ \(name, loc) ->
          withCString name $ glBindAttribLocation program $ fromIntegral loc
      glLinkProgram program

      success <- with (0 :: GLint) $ \ptr -> do
          glGetProgramiv program GL_LINK_STATUS ptr
          peek ptr
      return (program, success)

    if success == GL_FALSE
      then do
        err <- liftIO $ with (0 :: GLint) $ \ptr -> do
          glGetProgramiv program GL_INFO_LOG_LENGTH ptr
          logsize <- peek ptr
          infoLog <- allocaArray (fromIntegral logsize) $ \logptr -> do
            glGetProgramInfoLog program logsize nullPtr logptr
            peekArray (fromIntegral logsize) logptr
          return $ P.unlines [ "Could not link program"
                            , P.map (toEnum . fromEnum) infoLog
                            ]
        throwError err
      else do
        liftIO $ forM_ shaders glDeleteShader
        return program
--------------------------------------------------------------------------------
-- Loading shaders and compiling a program.
--------------------------------------------------------------------------------
loadSourcePaths :: MonadIO m
                => ShaderSteps (ts :: [*]) FilePath
                -> m (ShaderSteps ts ByteString)
loadSourcePaths = (ShaderSteps <$>) . mapM (liftIO . B.readFile) . unShaderSteps

compileSources
  :: forall m ts. (MonadIO m, MonadError String m, IsShaderType ts [GLenum])
  => ShaderSteps (ts :: [*]) ByteString
  -> m (ShaderSteps ts GLuint)
compileSources =
  (ShaderSteps <$>) . zipWithM (flip compileOGLShader) types . unShaderSteps
  where types = getShaderType (Proxy :: Proxy ts)

compileProgram
  :: (MonadIO m, MonadError String m, GetLits as [(String, Integer)])
  => Proxy (as :: [*])
  -> ShaderSteps (ts :: [*]) GLuint
  -> m GLuint
compileProgram p = compileOGLProgram (getSymbols p) . unShaderSteps

-- | Compile all shader programs and return a "sum renderer".
loadSimple2DShader :: (MonadIO m, MonadError String m) => m Simple2DShader
loadSimple2DShader = do
  vertName <- liftIO simple2dVertFilePath
  fragName <- liftIO simple2dFragFilePath
  let paths :: ShaderSteps '[VertexShader, FragmentShader] FilePath
      paths = ShaderSteps [vertName, fragName]
  sources <- loadSourcePaths paths
  shaders <- compileSources sources
  compileProgram (Proxy :: Proxy Simple2DAttribs) shaders
