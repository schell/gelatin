{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import           Gelatin
import           Linear

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------
picture :: Picture () (V2 Float, V4 Float) ()
picture = setGeometry $ fan $ do
  to (0, red)
  to (V2 100 0, green)
  to (100, blue)
  to (V2 0 100, white)

main :: IO ()
main = putStrLn "picture"
