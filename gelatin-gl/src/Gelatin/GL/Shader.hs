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
  , Simple3DShader
  , compileOGLShader
  , compileOGLProgram
  , loadSourcePaths
  , compileSources
  , compileProgram
  , loadProgram
  , loadSimple2DShader
  , loadSimple3DShader
  ) where

import           Control.Exception        (assert)
import           Control.Monad
import           Control.Monad.Except     (MonadError, throwError)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.ByteString.Char8    as B
import qualified Data.Foldable            as F
import           Data.Proxy               (Proxy (..))
import qualified Data.Vector.Storable     as S
import           Data.Vector.Unboxed      (Unbox, Vector)
import qualified Data.Vector.Unboxed      as V
import           Foreign.C.String
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.TypeLits             (KnownNat, KnownSymbol, natVal,
                                           symbolVal)
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Prelude                  hiding (init)
import           Prelude                  as P
--------------------------------------------------------------------------------
import           Gelatin
import           Gelatin.Shaders
import           Gelatin.Shaders.Simple2D (PrimType (..), Simple2DAttribs)
import           Gelatin.Shaders.Simple3D (Simple3DAttribs)
--------------------------------------------------------------------------------
import           Gelatin.GL.TH
--------------------------------------------------------------------------------


type Simple2DShader = GLuint
type Simple3DShader = GLuint
--------------------------------------------------------------------------------
-- IsShaderType instances
--------------------------------------------------------------------------------
instance IsShaderType VertexShader GLenum where
  getShaderType _ = GL_VERTEX_SHADER

instance IsShaderType FragmentShader GLenum where
  getShaderType _ = GL_FRAGMENT_SHADER
--------------------------------------------------------------------------------
-- Uniform marshaling function instances
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
-- Attribute buffering and toggling instances
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
    let ident = symbolVal (Proxy :: Proxy name)
        aloc  = seq ident $ fromIntegral $ natVal (Proxy :: Proxy loc)
    in (glEnableVertexAttribArray aloc, glDisableVertexAttribArray aloc)
--------------------------------------------------------------------------------
-- $opengl OpenGL shader only stuff
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
-- $shaderloading Loading shaders and compiling a program.
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

-- | Compile all 2D shader programs and return a 2D renderer.
loadProgram
  :: ( MonadIO m, MonadError String m
     , IsShaderType shadertypes [GLenum]
     , GetLits attributes [(String, Integer)]
     )
  => ShaderSteps (shadertypes :: [*]) FilePath
  -> Proxy (attributes :: [*])
  -> m GLuint
loadProgram shaderPaths pattribs =
  loadSourcePaths shaderPaths >>= compileSources >>= compileProgram pattribs

-- | Compile all 2D shader programs and return a 2D renderer.
loadSimple2DShader :: (MonadIO m, MonadError String m) => m Simple2DShader
loadSimple2DShader = do
  names <- liftIO $ sequence [simple3dVertFilePath, simple3dFragFilePath]
  let paths :: ShaderSteps '[VertexShader, FragmentShader] FilePath
      paths = ShaderSteps names
  loadProgram paths (Proxy :: Proxy Simple2DAttribs)

-- | Compile all 3D shader programs and return a 3D renderer.
loadSimple3DShader :: (MonadIO m, MonadError String m) => m Simple3DShader
loadSimple3DShader = do
  names <- liftIO $ sequence [simple3dVertFilePath, simple3dFragFilePath]
  let paths :: ShaderSteps '[VertexShader, FragmentShader] FilePath
      paths = ShaderSteps names
  loadProgram paths (Proxy :: Proxy Simple3DAttribs)
