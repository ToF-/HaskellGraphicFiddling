module Pythagore (pythShape) where
import Shape

pythShape :: Coord -> Angle -> Float -> Angle -> Shape
pythShape c phi l a = translate c $ rotate phi (0,0) $ scale (l,l) $ shape [(0,0),(1,0),(1,1),(x,y),(0,1)]
    where
    x = cos a  * cos a
    y = 1 + cos a * sin a

