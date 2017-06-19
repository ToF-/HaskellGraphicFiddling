module Pythagore where
import PythTree
import Shape

type Deviation = Float
pythagore :: Tree Deviation -> Float -> Coord -> Angle -> Tree Shape
pythagore Nil _ _ _ = Nil
pythagore (Fork d lt rt) s (x0,y0) phi =
    Fork (Rotate phi (x0,y0) $ Polygon [(x0,y0),(x0+s,y0),(x0+s,y0+s),(x,y),(x0,y0+s)])
        (pythagore lt (s*cosA) (x0,y0+s) (phi+alpha))
        (pythagore rt (s*sinA) (x,y) (phi+alpha-90))
    where
    alpha = (45+d) 
    cosA = cos (alpha*pi/180)
    sinA = sin (alpha*pi/180)
    x = x0+s*cosA * cosA
    y = y0+s+s*cosA * sinA

