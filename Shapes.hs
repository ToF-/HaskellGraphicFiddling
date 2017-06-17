module Shapes where

type Coord = (Float,Float)

data Shape = Shape [Coord]
           | Rotate Float Shape
           | Translate Coord Shape
           | Scale Float Shape
           | Shapes [Shape]
