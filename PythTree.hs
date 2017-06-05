import Graphics.UI.GLUT

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    window <- createWindow "Pythtree"
    displayCallback $= display
    reshapeCallback $= Just reshape
    mainLoop

reshape :: ReshapeCallback
reshape size = do 
    viewport $= (Position 0 0, size)

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    pythtree 3 0.0 0.0 0.0 0.0
    flush

