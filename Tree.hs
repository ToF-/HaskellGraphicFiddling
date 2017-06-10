module Tree (Tree, nilTree, mkTree, toList, fromList) where

data Tree a = Nil
            | Fork a (Tree a) (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Fork a l r) = Fork (f a) (fmap f l) (fmap f r)  

nilTree :: Tree a
nilTree = Nil

mkTree :: a -> Tree a -> Tree a -> Tree a 
mkTree a l r = Fork a l r

toList :: Tree a -> [a]
toList Nil = []
toList (Fork a l r) = a : (toList l) ++ (toList r)

fromList :: [a] -> Tree a
fromList [] = Nil
fromList [a] = mkTree a nilTree nilTree
fromList [a,b,c] = mkTree a (fromList [b]) (fromList [c]) 
