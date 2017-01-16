{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Gelatin.GL.TH where

import           Control.Exception   (assert)
import           Data.Proxy          (Proxy (..))
import           Foreign.C.String    (withCString)
import           GHC.TypeLits        (KnownSymbol, symbolVal)
import           Graphics.GL
import           Language.Haskell.TH

import           Gelatin.Shaders

genUniform :: TypeQ
           -- ^ The type of the uniform value.
           -- Most likely 'Bool', 'Float', 'V3', 'M44', etc.
           -> ExpQ
           -- ^ The function that marshals the value to the shader.
           -> DecsQ
genUniform typ func =
  [d|
  instance KnownSymbol name => HasGenFunc (Uniform name $typ) where
    type GenFunc (Uniform name $typ) = GLuint -> $typ -> IO ()
    genFunction _ program val = do
      let ident = symbolVal (Proxy :: Proxy name)
      loc <- withCString ident $ glGetUniformLocation program
      $func loc val
      glGetError >>= \case
        0 -> return ()
        e -> do
          putStrLn $ unwords [ "Could not update uniform"
                             , ident
                             , "with value"
                             , show val
                             , ", encountered error (" ++ show e ++ ")"
                             , show (GL_INVALID_OPERATION, "invalid operation")
                             , show (GL_INVALID_VALUE, "invalid value")
                             ]
          assert False $ return ()

  |]
