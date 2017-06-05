import Graphics.UI.GLUT

type Coord = (GLfloat,GLfloat)

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    window <- createWindow "Hello glut"
    displayCallback $= display
    reshapeCallback $= Just reshape
    mainLoop 

display :: DisplayCallback
display = do
    clear [ ColorBuffer ]
    (Size w h) <- get windowSize
    let r = fromIntegral h / fromIntegral w 
    preservingMatrix $ do
        scale' 0.5
        color green
        renderSquare r (0,0) 1
        color red
        renderSquare r (0,0) 0.75
        color blue
        renderSquare r (0,0) 0.5
    flush

reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)

renderSquare r (x,y) s = preservingMatrix $ do 
    renderPrimitive LineLoop $ vertex2s $ points r
    where 
    points r = [((x+0)*r,y+0),((x+s)*r,y+0),((x+s)*r,y+s),((x+0)*r,y+s)]
    
     
scale' :: GLfloat -> IO ()
scale' n = scale n n n

vertex2s :: [Coord] -> IO ()
vertex2s = mapM_ vertex2

vertex2 :: Coord -> IO ()
vertex2 (x,y) = vertex $ Vertex2 x y 

red :: Color3 GLfloat
red = Color3 0.8 0.1 0.1

green :: Color3 GLfloat
green = Color3 0 0.5 0.2

blue :: Color3 GLfloat
blue = Color3 0.1 0.1 0.8

