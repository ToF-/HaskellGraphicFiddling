import Test.Hspec
import Tree

main = hspec $ do
    describe "a tree" $ do
        it "can be nilTree" $ do
            toList nilTree `shouldBe`  ([] :: [Bool])
        it "can be one fork and two nilTree" $ do
            toList (mkTree "foo" nilTree nilTree)  `shouldBe` ["foo"]
        it "can be created from a list" $ do
            fromList "abcdefg" `shouldBe` 
                mkTree 'a' 
                  (mkTree 'b' 
                    (mkTree 'c' nilTree nilTree)
                    (mkTree 'd' nilTree nilTree))
                  (mkTree 'e'
                    (mkTree 'f' nilTree nilTree)
                    (mkTree 'g' nilTree nilTree))
        it "can be mapped a function over" $ do
            toList (fmap (+1) (fromList [42,17,23])) `shouldBe` [43,18,24]

        it "can grow to a certain level" $ do
            grow 0 4 id id  `shouldBe`  mkTree 4 nilTree nilTree
            toList (grow 2 1 (*2) ((+1).(*2))) `shouldBe` [1,2,4,5,3,6,7]
            


