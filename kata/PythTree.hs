module PythTree where

data Tree a = Nil
            | Fork a (Tree a) (Tree a)
    deriving (Show, Eq)

fromList :: [a] -> Tree a
fromList [] = Nil
fromList [a] = Fork a Nil Nil
fromList [a,b] = Fork a (Fork b Nil Nil) Nil
fromList (x:xs) = Fork x (fromList ls) (fromList rs)
    where
    ls = take h xs
    rs = drop h xs
    h  = length xs  `div` 2 

toList :: Tree a -> [a]
toList Nil = []
toList (Fork a lt rt) = a : toList lt ++ toList rt
