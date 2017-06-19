module Shape where
type Coord = (Float, Float)
type Angle = Float

data Shape = Polygon [Coord]
           | Rotate Angle Coord Shape
           | Color Float Float Float Shape
           | Translate Coord Shape
           | Shapes [Shape]
    deriving (Eq,Show)
