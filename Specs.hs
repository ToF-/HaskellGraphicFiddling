import Test.Hspec
import Tree
import Shape

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
        it "is a list of coords" $ do
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
