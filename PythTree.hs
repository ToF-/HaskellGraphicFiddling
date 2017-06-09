import Graphics.UI.GLUT
import System.Random
import Data.IORef

type Coord = (GLfloat, GLfloat)

delta :: Integer
delta = 25

depth :: Integer
depth = 15

main :: IO ()
main = do
    params <- newIORef (delta, depth)
    _ <- getArgsAndInitialize
    window <- createWindow "Pythtree"
    windowSize $= Size 800 800
    displayCallback $= display params
    reshapeCallback $= Just reshape 
    keyboardCallback $= Just (key params)
    scale 0.5 0.5 (0.5 :: GLfloat)
    mainLoop

key :: IORef (Integer, Integer) -> Char -> Position -> IO ()
key params 'd' _ = do
    (delta, depth) <- get params
    params $= (max (delta-1) 0, depth)
    display params
key params 'D' _ = do
    (delta, depth) <- get params
    params $= (min (delta+1) 45, depth)
    display params
key params 'f' _ = do
    (delta, depth) <- get params
    params $= (delta, max (depth-1) 1)
    display params
key params 'F' _ = do
    (delta, depth) <- get params
    params $= (delta, min (depth+1) 15)
    display params
key params ' ' _ = display params
key _ _   _ = return ()

reshape :: ReshapeCallback
reshape size = do 
    viewport $= (Position 0 0, size)

display :: IORef (Integer,Integer) -> DisplayCallback
display params = do
    (delta, depth) <- get params     
    clear [ColorBuffer]
    pythTree depth delta (Color3 0.5 0.5 0.5) (-0.25, -0.75) 0.35 0.0
    flush

pythTree :: Integer -> Integer -> Color3 GLfloat -> Coord -> GLfloat -> GLfloat -> IO ()
pythTree 0 _ _  _ _ _ = return ()
pythTree n delta (Color3 r g b) (x0,y0) len phi = do
    deviation <- getStdRandom $ randomR (-delta,delta)
    let alpha = (fromIntegral (45 + deviation))*pi/180
    let cosAlpha = cos alpha
    let sinAlpha = sin alpha
    let x4 = x0       + len * cosAlpha * cosAlpha
    let y4 = y0 + len + len * cosAlpha * sinAlpha
    let points = map (rotate (x0,y0) phi) [(x0,y0),(x0+len,y0),(x0+len,y0+len),(x0,y0+len),(x4,y4),(x0+len,y0+len),(x0,y0+len)] 
    let lines = if (n < 2) then [points!!0, points!!3, points!!1, points!!2, points!!3, points!!4, points!!4,points!!2] else [points!!0, points!!3, points!!1, points!!2]
    color (Color3 r g b)
    renderPrimitive Lines $ vertice2 $ lines
    pythTree (n-1) delta (Color3 r (g+0.3) (b-0.1)) (points!!3) (len*cosAlpha) (phi+alpha)
    pythTree (n-1) delta (Color3 r (g-0.1) (b+0.3)) (points!!4) (len*sinAlpha) (phi+alpha-0.5*pi)

    where
    rotate :: Coord -> GLfloat -> Coord -> Coord
    rotate (x0,y0) a (x,y) = (x*cosA - y*sinA + x0 - x0*cosA + y0 * sinA, x*sinA + y*cosA + y0 - x0*sinA - y0*cosA)
        where
        cosA = cos a
        sinA = sin a

    vertice2 :: [Coord] -> IO ()
    vertice2 = mapM_ vertex2

    vertex2 :: Coord -> IO ()
    vertex2 (x,y) = vertex $ Vertex2 x y 

    
       
    

