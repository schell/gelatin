{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import           Halive.Utils
import           Gelatin
import           Gelatin.Picture
import           Linear

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------
picture :: Picture () (V2 Float) Float (V2 Float, V4 Float) ()
picture = do
  move (V2 100 0)
  setGeometry $ fan $ do
    to (0, red)
    to (V2 100 0, green)
    to (100, blue)
    to (V2 0 100, white)


main :: IO ()
main = do
  let picSize = runPictureSize picture
  putStrLn $ unwords ["The example gelatin picture is ("
                     ,show picSize
                     ,") in size"
                     ]
