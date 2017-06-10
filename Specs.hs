import Test.Hspec
import Tree

main = hspec $ do
    describe "a tree" $ do
        it "can be nilTree" $ do
            toList nilTree `shouldBe`  ([] :: [Bool])
        it "can be one fork and two nilTree" $ do
            toList (mkTree "foo" nilTree nilTree)  `shouldBe` ["foo"]

        it "can be mapped a function over" $ do
            toList (fmap (+1) (fromList [42,17,23])) `shouldBe` [43,18,24]


