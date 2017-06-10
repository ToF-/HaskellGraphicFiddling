import Test.Hspec
import Tree

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
            


