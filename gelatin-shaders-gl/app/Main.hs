{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever, void)
import           Control.Monad.Trans.Either
import           Data.List                  (isSuffixOf)
import           Data.Proxy
import           Data.Time.Clock            (UTCTime (..), diffUTCTime,
                                             getCurrentTime)
import           Data.Vector.Unboxed        (Vector)
import qualified Data.Vector.Unboxed        as V
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Graphics.GL
import           Halive.Utils               (reacquire)
import           Linear
import           Options.Applicative
import           SDL
import           SDL.Video.OpenGL           (glGetDrawableSize)
import           System.Environment         (getArgs)
import           System.FilePath
import           System.FSNotify
--------------------------------------------------------------------------------
import           Gelatin.Shaders.GL         hiding (Color)
--------------------------------------------------------------------------------

ogl = defaultOpenGL{ glProfile = Core Debug 3 3 }

cfg = defaultWindow{ windowInitialSize = 800
                   , windowOpenGL = Just ogl
                   , windowResizable = True
                   , windowHighDPI = False
                   }

data ShaderValue = ShaderVertex | ShaderFragment deriving Show
data ShaderOption = ShaderOption { shaderOptionValue :: ShaderValue
                                 , shaderOptionPath  :: FilePath
                                 } deriving (Show)
data Options = Options { optionsShaderFiles :: [ShaderOption] } deriving (Show)

shaderParser :: Parser ShaderOption
shaderParser = argument readIt arg
  where arg = metavar "[SHADERFILE]" <> help "Shader file(s)"
        readIt = do
          let getType s
                | "vert" `isSuffixOf` s = return ShaderVertex
                | "frag" `isSuffixOf` s = return ShaderFragment
                | otherwise             = do
                let err = unwords [ "Filetype is not supported."
                                  , "Filetype must be one of vert or frag."
                                  , "Got"
                                  , show s
                                  ]
                readerError err

          file <- str
          typ  <- getType file
          if isValid file
            then return $ ShaderOption typ file
            else readerError $ unwords [ "SHADERFILE was invalid. Got"
                                       , show file
                                       ]
        sfile = str

filesParser :: Parser [ShaderOption]
filesParser = some shaderParser

optionsParser :: Parser Options
optionsParser = Options <$> filesParser

test :: Proxy a -> Proxy b -> Proxy '[a,b]
test _ _ = Proxy

--------------------------------------------------------------------------------
-- Uniforms
--------------------------------------------------------------------------------
type Projection = Uniform   "uprojection" (M44 Float)
updateProjection :: GLuint -> M44 Float -> IO ()
updateProjection = genFunction @Projection Proxy

type Time = Uniform "utime" Float
updateTime :: GLuint -> Float -> IO ()
updateTime = genFunction @Time Proxy

type Resolution = Uniform "uresolution" (V2 Float)
updateResolution :: GLuint -> V2 Float -> IO ()
updateResolution = genFunction @Resolution Proxy

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------
type Position   = Attribute "position"   (V2 Float) 0
bufferPosition :: GLint -> GLuint -> Vector (V2 Float) -> IO GLuint
bufferPosition = genFunction @(AttributeBuffering Position) Proxy

type Color      = Attribute "color"      (V4 Float) 1
bufferColor :: GLint -> GLuint -> Vector (V4 Float) -> IO GLuint
bufferColor = genFunction @(AttributeBuffering Color) Proxy

loadColorTriangle :: GLuint -> IO (Int, GLuint, GLuint)
loadColorTriangle vao = do
  let (vs, cs) = V.unzip $ runVertices $
        tri (0, red) (V2 400 0, green) (400, blue)
  (3,,) <$> bufferPosition 2 vao vs
        <*> bufferColor 4 vao cs

loadColorQuad :: GLuint -> V2 Float -> IO (Int, GLuint, GLuint)
loadColorQuad vao (V2 w h) = do
  let (vs,cs) = V.unzip $ runVertices $ do
        tri (0, red) (V2 w 0, green) (V2 w h, blue)
        tri (0, red) (V2 w h, blue) (V2 0 h, green)
  (6,,) <$> bufferPosition 2 vao vs
        <*> bufferColor 4 vao cs

main :: IO ()
main = do
  let vert = "shaders" </> "bookofshaders" </> "05.vert"
      frag = "shaders" </> "bookofshaders" </> "05.frag"

  window <- reacquire 0 $ do
    initializeAll
    window <- createWindow "shaders" cfg
    _      <- glCreateContext window
    glClearColor 0 0 0 1
    return window

  let paths :: ShaderSteps '[VertexShader, FragmentShader] FilePath
      paths = ShaderSteps [vert, frag]
      attrb = Proxy :: Proxy '[Position]

  runEitherT (loadProgram paths attrb) >>= \case
    Left err -> print err
    Right p  -> do
      glUseProgram p
      start <- getCurrentTime
      loop window p start
  where loop window p start = do
          threadDelay 10
          void pollEvents

          glClear GL_COLOR_BUFFER_BIT

          resolution@(V2 w h) <- (fromIntegral <$>) <$> glGetDrawableSize window
          updateResolution p resolution

          updateProjection p $ ortho 0 w h 0 0 1
          now <- getCurrentTime
          updateTime p $ realToFrac $ diffUTCTime now start

          [vao] <- allocaArray 1 $ \ptr -> do
            glGenVertexArrays 1 ptr
            peekArray 1 ptr
          (n, positionBuffer, colorBuffer) <- loadColorQuad vao 1
          glBindVertexArray vao
          glDrawArrays GL_TRIANGLES 0 $ fromIntegral n
          glBindVertexArray 0

          glSwapWindow window
          withArray [vao] $ glDeleteVertexArrays 1
          withArray [colorBuffer, positionBuffer] $ glDeleteBuffers 2
          loop window p start
