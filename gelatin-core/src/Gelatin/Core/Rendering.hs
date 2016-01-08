module Gelatin.Core.Rendering where

import Gelatin.Core.Transform

data Rendering = Rendering RenderFunction CleanupFunction
type RenderFunction = Transform -> IO ()

type CleanupFunction = IO ()

instance Monoid Rendering where
    mempty = Rendering (const $ return ()) (return ())
    (Rendering ar ac) `mappend` (Rendering br bc) =
        Rendering (\t -> ar t >> br t) (ac >> bc)

runRendering :: Transform -> Rendering -> IO ()
runRendering t (Rendering f _) = f t

cleanRendering :: Rendering -> IO ()
cleanRendering (Rendering _ c) = c
