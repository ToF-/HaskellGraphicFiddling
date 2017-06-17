module Tree  where

data Tree a = Nil
            | Fork a (Tree a) (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Fork a l r) = Fork (f a) (fmap f l) (fmap f r)  

toList :: Tree a -> [a]
toList Nil = []
toList (Fork a l r) = a : (toList l) ++ (toList r)

fromList :: [a] -> Tree a
fromList [] = Nil
fromList (a:as) = Fork a (fromList ls) (fromList rs)
    where 
    ls = take n as
    rs = drop n as
    n  = length as `div` 2

grow :: Int -> a -> (a -> a) -> (a -> a) -> Tree a
grow 0 a _ _ = Fork a Nil Nil
grow n a f g = Fork a (grow (n-1) (f a) f g) (grow (n-1) (g a) f g)
