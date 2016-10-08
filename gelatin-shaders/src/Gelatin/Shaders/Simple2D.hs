module Gelatin.Shaders.Simple2D where

import Gelatin
import Gelatin.Shaders.Common
import Data.Word (Word32)
import Data.Maybe (isJust, fromMaybe)

--------------------------------------------------------------------------------
-- $layout
-- Attributes layout locations are unique and global.
--------------------------------------------------------------------------------
data Simple2DAttrib = PositionLoc
                    | ColorLoc
                    | UVLoc
                    | BezLoc
                    | NextLoc
                    | PrevLoc
                    | BezUVLoc
                    deriving (Show, Eq, Ord, Enum, Bounded)

allAttribs :: [Simple2DAttrib]
allAttribs = [minBound..maxBound]

simple2DAttribIdentifier :: Simple2DAttrib -> String
simple2DAttribIdentifier PositionLoc = "position"
simple2DAttribIdentifier ColorLoc    = "color"
simple2DAttribIdentifier UVLoc       = "uv"
simple2DAttribIdentifier BezLoc      = "bez"
simple2DAttribIdentifier NextLoc     = "next"
simple2DAttribIdentifier PrevLoc     = "previous"
simple2DAttribIdentifier BezUVLoc    = "bezuv"
--------------------------------------------------------------------------------
-- $uniforms
-- Uniform Helper Types
--------------------------------------------------------------------------------
data PrimType = PrimTri
              | PrimBez
              | PrimLine
              | PrimMask
              deriving (Show, Eq, Enum, Ord, Bounded)
--------------------------------------------------------------------------------
-- Updating uniforms
--------------------------------------------------------------------------------
data Simple2DUniform =
    UniformPrimType PrimType
  | UniformProjection (M44 Float)
  | UniformModelView (M44 Float)
  | UniformThickness Float
  | UniformFeather Float
  | UniformSumLength Float
  | UniformLineCaps (LineCap,LineCap)
  | UniformHasUV Bool
  | UniformSampler Word32
  | UniformMainTex Word32
  | UniformMaskTex Word32
  | UniformAlpha Float
  | UniformMult (V4 Float)
  | UniformShouldReplaceColor Bool
  | UniformReplaceColor (V4 Float)
  deriving (Show, Ord, Eq)

allSimple2DUniforms :: [Simple2DUniform]
allSimple2DUniforms = [UniformPrimType PrimTri
              ,UniformProjection 0
              ,UniformModelView 0
              ,UniformThickness 0
              ,UniformFeather 0
              ,UniformSumLength 0
              ,UniformLineCaps (LineCapNone, LineCapNone)
              ,UniformHasUV False
              ,UniformSampler 0
              ,UniformMainTex 0
              ,UniformMaskTex 0
              ,UniformAlpha 1
              ,UniformMult 1
              ,UniformShouldReplaceColor False
              ,UniformReplaceColor 0
              ]

applyAlpha :: Float -> [Simple2DUniform] -> [Simple2DUniform]
applyAlpha a us = UniformAlpha a : filter f us
  where f (UniformAlpha _) = False
        f _ = True

applyMult :: V4 Float -> [Simple2DUniform] -> [Simple2DUniform]
applyMult v us = UniformMult v : filter f us
  where f (UniformMult _) = False
        f _ = True

simple2DUniformIdentifier :: Simple2DUniform -> String
simple2DUniformIdentifier (UniformPrimType _)           = "primitive"
simple2DUniformIdentifier (UniformProjection _)         = "projection"
simple2DUniformIdentifier (UniformModelView _)          = "modelview"
simple2DUniformIdentifier (UniformThickness _)          = "thickness"
simple2DUniformIdentifier (UniformFeather _)            = "feather"
simple2DUniformIdentifier (UniformSumLength _)          = "sumlength"
simple2DUniformIdentifier (UniformLineCaps _)           = "cap"
simple2DUniformIdentifier (UniformHasUV _)              = "hasUV"
simple2DUniformIdentifier (UniformSampler _)            = "sampler"
simple2DUniformIdentifier (UniformMainTex _)            = "mainTex"
simple2DUniformIdentifier (UniformMaskTex _)            = "maskTex"
simple2DUniformIdentifier (UniformAlpha _)              = "alpha"
simple2DUniformIdentifier (UniformMult _)               = "mult"
simple2DUniformIdentifier (UniformShouldReplaceColor _) = "shouldColorReplace"
simple2DUniformIdentifier (UniformReplaceColor _)       = "replaceColor"

uniformsForTris :: M44 Float -> M44 Float -> Bool -> Float -> V4 Float
                -> Maybe (V4 Float) -> [Simple2DUniform]
uniformsForTris pj mv hasUV a m mr = map f allSimple2DUniforms
  where f (UniformPrimType _) = UniformPrimType PrimTri
        f (UniformProjection _) = UniformProjection pj
        f (UniformModelView _) = UniformModelView mv
        f (UniformHasUV _) = UniformHasUV hasUV
        f (UniformAlpha _) = UniformAlpha a
        f (UniformMult _) = UniformMult m
        f s@(UniformShouldReplaceColor _) = if isJust mr
                                              then UniformShouldReplaceColor True
                                              else s
        f (UniformReplaceColor c) = UniformReplaceColor $ fromMaybe c mr
        f x = x
{-# INLINE uniformsForTris #-}

uniformsForBezs :: M44 Float -> M44 Float -> Bool -> Float -> V4 Float
                -> Maybe (V4 Float) -> [Simple2DUniform]
uniformsForBezs pj mv hasUV a m mr = map f $ uniformsForTris pj mv hasUV a m mr
  where f (UniformPrimType _) = UniformPrimType PrimBez
        f x = x
{-# INLINE uniformsForBezs #-}

uniformsForLines :: M44 Float -> M44 Float -> Bool -> Float -> V4 Float
                 -> Maybe (V4 Float) -> Float -> Float -> Float
                 -> (LineCap,LineCap) -> [Simple2DUniform]
uniformsForLines pj mv hasUV a m mr thickness feather sumlength caps =
  map f $ uniformsForTris pj mv hasUV a m mr
    where f (UniformPrimType _) = UniformPrimType PrimLine
          f (UniformThickness _) = UniformThickness thickness
          f (UniformFeather _) = UniformFeather feather
          f (UniformSumLength _) = UniformSumLength sumlength
          f (UniformLineCaps _) = UniformLineCaps caps
          f x = x
{-# INLINE uniformsForLines #-}

uniformsForMask :: M44 Float -> M44 Float -> Float -> V4 Float -> Word32
                -> Word32 -> [Simple2DUniform]
uniformsForMask pj mv a m main mask = map f $ uniformsForTris pj mv True a m
                                                                Nothing
  where f (UniformPrimType _) = UniformPrimType PrimMask
        f (UniformMainTex _) = UniformMainTex main
        f (UniformMaskTex _) = UniformMaskTex mask
        f x = x
{-# INLINE uniformsForMask #-}
