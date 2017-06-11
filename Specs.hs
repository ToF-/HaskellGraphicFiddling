import Test.Hspec
import Tree
import Shape
import Pythagore

rounded :: Coord -> Coord
rounded (x,y) = (roundedFloat x, roundedFloat y)
    where
    roundedFloat n = (fromIntegral (round (n * 10000))) / 10000
main = hspec $ do
    describe "a tree" $ do
        it "can be nil" $ do
            toList nil `shouldBe`  ([] :: [Bool])
        it "can be one fork and two nil" $ do
            toList (tree "foo" nil nil)  `shouldBe` ["foo"]
        it "can be created from a list" $ do
            fromList "abcdefg" `shouldBe` 
                tree 'a' 
                  (tree 'b' 
                    (tree 'c' nil nil)
                    (tree 'd' nil nil))
                  (tree 'e'
                    (tree 'f' nil nil)
                    (tree 'g' nil nil))
        it "can be mapped a function over" $ do
            let t = tree 42 (tree 17 nil nil) (tree 23 nil nil)
            fmap (+1) t `shouldBe` tree 43 (tree 18 nil nil) (tree 24 nil nil)

        it "can grow to a certain level" $ do
            grow 0 4 id id  `shouldBe`  tree 4 nil nil
            grow 2 1 (*2) ((+1).(*2)) `shouldBe` 
                tree 1
                    (tree 2
                        (tree 4 nil nil)
                        (tree 5 nil nil))
                    (tree 3
                        (tree 6 nil nil)
                        (tree 7 nil nil))
            
    describe "a shape" $ do
        let s = shape [(0,0), (1,0), (1,1)]
        it "has a list of coords" $ do
            coords s  `shouldBe` [(0,0), (1,0), (1,1)]
        it "can be translated" $ do
            let s' = translate (-2,4) s
            coords s'  `shouldBe` [(-2,4),(-1,4),(-1,5)]
        it "can be rotated around a point" $ do
            let s = shape [(0,0),(1,0),(1,1)]
            let s'  = rotate (pi/2) (0,0) s
            let s'' = rotate (-pi/2) (0,0) s
            map rounded (coords s') `shouldBe` [(0,0),(0,1),(-1,1)]
            map rounded (coords s'') `shouldBe` [(0,0),(0,-1),(1,-1)]
        it "can be scaled on x and y" $ do
            let s' = scale (2,0.5) s
            coords s'  `shouldBe` [(0,0), (2,0), (2,0.5)]

    describe "pythShape" $ do
        it "creates a shape from a position, an angle, a length and an new angle" $ do
            let s = pythShape (1,2) 0 1 (pi/4)
            let t = pythShape (0,0) (pi/2) 1 (pi/4)
            map rounded (coords s) `shouldBe` [(1,2),(2,2),(2,3),(1.5,3.5),(1,3)]
            map rounded (coords t) `shouldBe` [(0,0),(0,1),(-1,1),(-1.5,0.5),(-1,0)]
