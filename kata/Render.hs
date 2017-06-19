import Graphics.UI.GLUT as GL
import Shape as S
import PythTree
import Pythagore

p = S.Shapes $ toList $ pythagore (fromList [0,0,0,0,0,0,0]) 0.25 (0,0) 0
main = do
    _ <- getArgsAndInitialize 
    window <- createWindow "Render"
    windowSize $= Size 1000 1000
    displayCallback $= display
    reshapeCallback $= Just reshape
    mainLoop

display :: IO ()
display = do
    clear [ColorBuffer]
    displayShape p
    flush

reshape :: ReshapeCallback
reshape size = do 
    viewport $= (Position 0 0, size)

displayShape :: Shape -> IO ()
displayShape (S.Polygon cs) = renderPrimitive GL.Polygon $ vertice2 cs
displayShape (S.Rotate a (x,y) s) = preservingMatrix $ do
    translate $ Vector3 x y 0
    rotate a $ Vector3 0 0 1
    translate $ Vector3 (-x) (-y) 0
    displayShape s 
displayShape (S.Color r g b s) = preservingMatrix $ do
    color (Color3 r g b)
    displayShape s
displayShape (S.Translate (x,y) s) = preservingMatrix $ do
    translate $ Vector3 x y 0
    displayShape s
displayShape (S.Shapes ss) = mapM_ displayShape ss

vertice2 :: [Coord] -> IO ()
vertice2 = mapM_ vertex2
    where
    vertex2 :: Coord -> IO ()
    vertex2 (x,y) = vertex $ Vertex2 x y 
