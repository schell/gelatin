{-# LANGUAGE TemplateHaskell #-}
module Shader.Internal where

import Data.FileEmbed
import Data.ByteString.Char8

vertSourceGeom :: ByteString
vertSourceGeom = $(embedFile "shaders/vert.glsl")

fragSourceGeom :: ByteString
fragSourceGeom = $(embedFile "shaders/frag.glsl")

vertSourceBezier :: ByteString
vertSourceBezier = $(embedFile "shaders/bezier.vert")

fragSourceBezier :: ByteString
fragSourceBezier = $(embedFile "shaders/bezier.frag")
