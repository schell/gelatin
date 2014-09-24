{-# LANGUAGE OverloadedStrings #-}
module Gelatin.Shaders.Parser where

import Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8
import Control.Applicative
import Gelatin.Shaders.GLSL
import Prelude as Prelude

data ShaderType = Uniform TType ByteString
                | Attribute TType ByteString
                | Varying TType ByteString
                deriving (Show)

shaderTypes :: Parser [ShaderType]
shaderTypes = fmap Prelude.concat $
    many' (uniform   <|>
           attribute <|>
           varying   <|>
           otherStatements)

uniform :: Parser [ShaderType]
uniform = shaderTypeParser Uniform "uniform"

attribute :: Parser [ShaderType]
attribute = shaderTypeParser Attribute "attribute"

varying :: Parser [ShaderType]
varying = shaderTypeParser Varying "varying"

otherStatements :: Parser [ShaderType]
otherStatements = ([]) <$ takeTill (== ';') <* char ';'

shaderTypeParser :: (TType -> ByteString -> ShaderType) -> ByteString -> Parser [ShaderType]
shaderTypeParser f s = do
    _ <- string s
    skipSpace
    t <- ttype
    skipSpace
    n <- takeTill (\c -> c == ';' || isSpace c)
    skipSpace
    _ <- char ';'
    skipSpace
    return [f t n]

ttype :: Parser TType
ttype = do
    s <- takeTill isSpace
    let mt = glslTypeByString $ unpack s
    case mt of
        Nothing -> fail $ "Could not parse glsl type: " ++ unpack s
        Just t  -> return t
