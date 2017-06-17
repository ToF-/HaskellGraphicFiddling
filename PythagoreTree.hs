module PythagoreTree where
import Tree
import Shapes

type Angle = Float
type Size  = Float
type Position = (Float,Float)
type Deviation = Float

pythagoreTree :: (Tree Deviation) -> Position -> Angle -> Size -> Tree Shape
pythagoreTree Nil _ _ _ = Nil
pythagoreTree (Fork d lt rt) (x,y) phi s =
    Fork (Translate (x,y) (Scale s (Rotate (degrees phi) (Shape points))))
        (pythagoreTree lt (x+0,y+s) (phi + alpha) ((s*cosA)/s))
        (pythagoreTree rt (x+s*cosA*cosA,y+s+s*cosA*sinA) (phi + alpha - pi/2) ((s*sinA)/s))
    where 
    alpha = pi / 4 + d
    cosA = cos alpha
    sinA = sin alpha
    degrees a = a * 180 / pi
    points = [(0,0),(1,0),(1,1),(cosA*cosA,1+cosA*sinA),(0,1)]
