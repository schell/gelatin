module Gelatin.Shaders.GLSL where

import Data.Char (toLower)

glslTypeByString :: String -> Maybe TType
glslTypeByString str = lookup str list
    where list  = zip (map glslTypeString types) types
          types = [minBound..maxBound]

glslTypeString :: TType -> String
glslTypeString t = (toLower s) : str
    where tstr    = tail $ show t
          (s:str) = tstr

data TType = TVoid
           | TBool
           | TInt
           | TUint
           | TFloat
           | TDouble
           | TVec2 | TVec3 | TVec4
           | TDvec2 | TDvec3 | TDvec4
           | TBvec2 | TBvec3 | TBvec4
           | TIvec2 | TIvec3 | TIvec4
           | TUvec2 | TUvec3 | TUvec4
           | TMat2 | TMat3 | TMat4 | TMat2x2
           | TMat2x3 | TMat2x4 | TMat3x2 | TMat3x3 | TMat3x4 | TMat4x2 | TMat4x3 | TMat4x4
           | TDmat2 | TDmat3 | TDmat4 | TDmat2x2
           | TDmat2x3 | TDmat2x4 | TDmat3x2 | TDmat3x3 | TDmat3x4 | TDmat4x2 | TDmat4x3 | TDmat4x4
           | TSampler1D | TSampler2D | TSampler3D
           | TImage1D | TImage2D | TImage3D
           | TSamplerCube
           | TImageCube
           | TSampler2DRect
           | TImage2DRect
           | TSampler1DArray | TSampler2DArray
           | TImage1DArray | TImage2DArray
           | TSamplerBuffer
           | TImageBuffer
           | TSampler2DMS
           | TImage2DMS
           | TSampler2DMSArray
           | TImage2DMSArray
           | TSamplerCubeArray
           | TImageCubeArray
           | TSampler1DShadow | TSampler2DShadow | TSampler2DRectShadow
           | TSampler1DArrayShadow | TSampler2DArrayShadow
           | TSamplerCubeShadow
           | TSamplerCubeArrayShadow
           | TIsampler1D | TIsampler2D | TIsampler3D
           | TIimage1D | TIimage2D | TIimage3D
           | TIsamplerCube
           | TIimageCube
           | TIsampler2DRect
           | TIimage2DRect
           | TIsampler1DArray | TIsampler2DArray
           | TIimage1DArray | TIimage2DArray
           | TIsamplerBuffer
           | TIimageBuffer
           | TIsampler2DMS
           | TIimage2DMS
           | TIsampler2DMSArray
           | TIimage2DMSArray
           | TIsamplerCubeArray
           | TIimageCubeArray
           | TAtomic_uint
           | TUsampler1D | TUsampler2D | TUsampler3D
           | TUimage1D | TUimage2D | TUimage3D
           | TUsamplerCube
           | TUimageCube
           | TUsampler2DRect
           | TUimage2DRect
           | TUsampler1DArray | TUsampler2DArray
           | TUimage1DArray | TUimage2DArray
           | TUsamplerBuffer
           | TUimageBuffer
           | TUsampler2DMS
           | TUimage2DMS
           | TUsampler2DMSArray
           | TUimage2DMSArray
           | TUsamplerCubeArray
           | TUimageCubeArray
           deriving (Show, Ord, Eq, Enum, Bounded)
