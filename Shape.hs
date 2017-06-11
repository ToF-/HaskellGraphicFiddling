module Shape (Shape, Coord, shape, coords, translate, rotate, scale) where

type Coord = (Float, Float)
type Angle = Float
data Shape = Shape [Coord]
    deriving (Eq, Show)

shape :: [Coord] -> Shape
shape = Shape

coords :: Shape -> [Coord]
coords (Shape cs) = cs

translate :: Coord -> Shape -> Shape
translate (u,v) (Shape cs) = Shape $ map (\(x,y)->(x+u,y+v)) cs

rotate :: Angle -> Coord -> Shape -> Shape
rotate a c (Shape cs) = Shape $ map (rotateCoord a c) cs  
    where 
    rotateCoord :: Angle -> Coord -> Coord -> Coord
    rotateCoord a (x0,y0) (x,y) = ((x*cosA) - (y*sinA) + (x0 - x0)*cosA + (y0*sinA), 
                                   (x*sinA) + (y*cosA) + (y0 - x0)*sinA - (y0*cosA))
        where
        cosA = cos a
        sinA = sin a

scale :: Coord -> Shape -> Shape
scale c (Shape cs) = Shape $ map (scaleCoord c) cs
    where
    scaleCoord (u,v) (x,y) = (x*u,y*v)
