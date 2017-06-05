import Graphics.UI.GLUT
import System.Random

type Coord = (GLfloat, GLfloat)

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    window <- createWindow "Pythtree"
    windowSize $= Size 800 800
    displayCallback $= display
    reshapeCallback $= Just reshape
    mainLoop

reshape :: ReshapeCallback
reshape size = do 
    viewport $= (Position 0 0, size)

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    gen <- newStdGen
    pythTree 7 gen (-0.25, -0.75) 0.35 0.0
    flush

delta :: Integer
delta = 15

pythTree :: Integer -> StdGen -> Coord -> GLfloat -> GLfloat -> IO ()
pythTree 0 _ _ _ _ = return ()
pythTree n gen (x0,y0) a phi = do
    deviation <- getStdRandom $ randomR (-delta,delta)
    let alpha = (fromIntegral (45 + deviation))*pi/180
    let calpha = cos alpha
    let salpha = sin alpha
    let c = a * calpha 
    let b = a * salpha
    let x4 = x0 + c * calpha
    let y4 = y0+a + c * salpha
    let cphi = cos phi
    let sphi = sin phi
    let c1 = x0 - x0 * cphi + y0 * sphi
    let c2 = y0 - x0 * sphi - y0 * cphi
    let points = map (\(x,y) -> (x*cphi - y*sphi + c1, x*sphi + y*cphi + c2))
         [(x0,y0),(x0+a,y0),(x0+a,y0+a),(x0,y0+a),(x4,y4),(x0+a,y0+a),(x0,y0+a)] 
    renderPrimitive LineLoop $ vertice2 $ points
    gen' <- newStdGen
    pythTree (n-1) gen'  (points!!3) c (phi+alpha)
    gen'' <- newStdGen
    pythTree (n-1) gen'' (points!!4) b (phi+alpha-0.5*pi)
    
    

vertice2 :: [Coord] -> IO ()
vertice2 = mapM_ vertex2

vertex2 :: Coord -> IO ()
vertex2 (x,y) = vertex $ Vertex2 x y 

    
       
    

