{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Gelatin.FreeType2.Utils (
   module FT
 , FreeTypeT
 , FreeTypeIO
 , getAdvance
 , getCharIndex
 , getLibrary
 , getKerning
 , glyphFormatString
 , hasKerning
 , loadChar
 , loadGlyph
 , newFace
 , setCharSize
 , setPixelSizes
 , withFreeType
 , runFreeType
) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Control.Monad (unless)
import           Graphics.Rendering.FreeType.Internal                   as FT
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes    as FT
import           Graphics.Rendering.FreeType.Internal.Library           as FT
import           Graphics.Rendering.FreeType.Internal.FaceType          as FT
import           Graphics.Rendering.FreeType.Internal.Face as FT hiding (generic)
import           Graphics.Rendering.FreeType.Internal.GlyphSlot         as FT
import           Graphics.Rendering.FreeType.Internal.Bitmap            as FT
import           Graphics.Rendering.FreeType.Internal.Vector            as FT
import           Foreign                                                as FT
import           Foreign.C.String                                       as FT

type FreeTypeT m = EitherT String (StateT FT_Library m)
type FreeTypeIO = FreeTypeT IO

glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString fmt
    | fmt == ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
    | fmt == ft_GLYPH_FORMAT_OUTLINE = "ft_GLYPH_FORMAT_OUTLINE"
    | fmt == ft_GLYPH_FORMAT_PLOTTER = "ft_GLYPH_FORMAT_PLOTTER"
    | fmt == ft_GLYPH_FORMAT_BITMAP = "ft_GLYPH_FORMAT_BITMAP"
    | otherwise = "ft_GLYPH_FORMAT_NONE"

liftE :: MonadIO m => IO (Either FT_Error a) -> FreeTypeT m a
liftE f = (liftIO f) >>= \case
  Left e  -> left $ "FreeType2 error:" ++ (show e)
  Right a -> right a

runIOErr :: MonadIO m => IO FT_Error -> FreeTypeT m ()
runIOErr f = do
  e <- liftIO f
  unless (e == 0) $ fail $ "FreeType2 error:" ++ (show e)

runFreeType :: MonadIO m => FreeTypeT m a -> m (Either String (a, FT_Library))
runFreeType f = do
  (e,lib) <- liftIO $ alloca $ \p -> do
    e <- ft_Init_FreeType p
    lib <- peek p
    return (e,lib)
  if e /= 0
    then do
      _ <- liftIO $ ft_Done_FreeType lib
      return $ Left $ "Error initializing FreeType2:" ++ show e
    else (fmap (,lib)) <$> evalStateT (runEitherT f) lib

withFreeType :: MonadIO m => Maybe FT_Library -> FreeTypeT m a -> m (Either String a)
withFreeType Nothing f = runFreeType f >>= \case
  Left e -> return $ Left e
  Right (a,lib) -> do
    _ <- liftIO $ ft_Done_FreeType lib
    return $ Right a
withFreeType (Just lib) f = evalStateT (runEitherT f) lib

getLibrary :: MonadIO m => FreeTypeT m FT_Library
getLibrary = lift get

newFace :: MonadIO m => FilePath -> FreeTypeT m FT_Face
newFace fp = do
  ft <- lift get
  liftE $ withCString fp $ \str ->
    alloca $ \ptr -> ft_New_Face ft str 0 ptr >>= \case
      0 -> Right <$> peek ptr
      e -> return $ Left e

setCharSize :: (MonadIO m, Integral i) => FT_Face -> i -> i -> i -> i -> FreeTypeT m ()
setCharSize ff w h dpix dpiy = runIOErr $
  ft_Set_Char_Size ff (fromIntegral w)    (fromIntegral h)
                      (fromIntegral dpix) (fromIntegral dpiy)

setPixelSizes :: (MonadIO m, Integral i) => FT_Face -> i -> i -> FreeTypeT m ()
setPixelSizes ff w h =
  runIOErr $ ft_Set_Pixel_Sizes ff (fromIntegral w) (fromIntegral h)

getCharIndex :: (MonadIO m, Integral i)
             => FT_Face -> i -> FreeTypeT m FT_UInt
getCharIndex ff ndx = liftIO $ ft_Get_Char_Index ff $ fromIntegral ndx

loadGlyph :: MonadIO m => FT_Face -> FT_UInt -> FT_Int32 -> FreeTypeT m ()
loadGlyph ff fg flags = runIOErr $ ft_Load_Glyph ff fg flags

loadChar :: MonadIO m => FT_Face -> FT_ULong -> FT_Int32 -> FreeTypeT m ()
loadChar ff char flags = runIOErr $ ft_Load_Char ff char flags

hasKerning :: MonadIO m => FT_Face -> FreeTypeT m Bool
hasKerning = liftIO . ft_HAS_KERNING

getKerning :: MonadIO m => FT_Face -> FT_UInt -> FT_UInt -> FT_Kerning_Mode -> FreeTypeT m (Int,Int)
getKerning ff prevNdx curNdx flags = liftE $ alloca $ \ptr -> do
  ft_Get_Kerning ff prevNdx curNdx (fromIntegral flags) ptr >>= \case
    0 -> do FT_Vector x y <- peek ptr
            return $ Right (fromIntegral x, fromIntegral y)
    e -> return $ Left e

getAdvance :: MonadIO m => FT_GlyphSlot -> FreeTypeT m (Int,Int)
getAdvance slot = do
  FT_Vector x y <- liftIO $ peek $ advance slot
  liftIO $ print ("v",x,y)
  return (fromIntegral x, fromIntegral y)
