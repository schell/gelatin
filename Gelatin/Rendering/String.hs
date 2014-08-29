module Gelatin.Rendering.String where

import Gelatin.Core
import Control.Monad.Free
import Control.Monad.Free.Church

pad :: Int -> String
pad i = replicate (2*i) ' '

--showCommand :: Int -> Free (DrawCommand r) () -> String
--showCommand _ (Pure ()) = ""
--showCommand i (Free (Fill c vs n)) =
--    unlines [ pad i ++ "Fill:"
--            , pad i ++ show vs
--            , pad i ++ show c
--            ] ++ pad i ++ showCommand i n
--showCommand i (Free (Gradient cs vs n)) =
--    unlines [ pad i ++ "Gradient:"
--            , pad i ++ show vs
--            , pad i ++ show cs
--            ] ++ pad i ++ showCommand i n
--showCommand i (Free (WithTransform t d n)) =
--    unlines [ pad i ++ "WithTransform:"
--            , pad i ++ show t
--            , pad i ++ (showCommand (i+1) $ fromF d)
--            ] ++ pad i ++ showCommand i n
--showCommand i (Free (WithTexture src d n)) =
--    unlines [ pad i ++ "WithTexture:" ++ show src
--            , pad i ++ (showCommand (i+1) $ fromF d)
--            ] ++ pad i ++ showCommand i n
--showCommand i (Free (TexTris vs ts n)) =
--    unlines [ pad i ++ "TexTris:"
--            , pad i ++ show vs
--            , pad i ++ show ts
--            ] ++ pad i ++ showCommand i n
--showCommand i (Free (RawRendering _ n)) =
--    unlines [ pad i ++ "OtherRendering:"
--            ] ++ pad i ++ showCommand i n
--
--showDrawing' :: Show a => Int -> Drawing a () -> String
--showDrawing' i = showCommand i . fromF
--
--showDrawing :: Show a => Drawing a () -> String
--showDrawing = showDrawing' 0
