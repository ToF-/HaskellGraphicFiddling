import Graphics.UI.GLUT
import Shapes

main :: IO ()
main = do
    getArgsAndInitialize
    window <- createWindow "Pythtree"
    windowSize $= Size 800 800
    displayCallback $= display
    reshapeCallback $= Just reshape
    keyboardCallback $= Nothing
    mainLoop

reshape :: ReshapeCallback
reshape size = viewport $= (Position 0 0, size)

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    scale 0.5 0.5 (0.5:: GLfloat)
    drawShape shape
    flush
    where
    drawShape :: Shape -> IO ()
    drawShape (Shape coords) = renderPrimitive Polygon (vertices2 coords)
    drawShape (Rotate a s)   = preservingMatrix $ do
        rotate a $ Vector3 0 0 1
        drawShape s
    drawShape (Translate (x,y) s) = preservingMatrix $ do
        translate $ Vector3 x y 0 
        drawShape s
    drawShape (Scale n s) = preservingMatrix $ do
        scale n n 0 
        drawShape s
    drawShape (Shapes ss) = mapM_ drawShape ss 

    shape :: Shape
    shape = Shapes [
        Scale 0.4 $ Translate (-0.5,-1.5) $ Rotate 30 $ Shape [(0,0),(1,0),(1,1),(0.5,1.5),(0,1)],
        Rotate (-30) $ Shape [(0,0),(1,0),(1,1),(0.5,1.5),(0,1)]]

    vertices2 = mapM_ vertex2

    vertex2 (x,y) = vertex $ Vertex2 x y
