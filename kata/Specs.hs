import Test.Hspec
import PythTree

main = hspec $ do
    describe "a tree" $ do
        it "can be built from an empty list" $ do
            let t = fromList [] :: Tree Integer
            t  `shouldBe` Nil 

        it "can be build from a list of 1 thing" $ do
            let t = fromList [42] 
            t `shouldBe` Fork 42 Nil Nil 

        it "can be built from a list of n things" $ do
            fromList [42,17]  `shouldBe` Fork 42 (Fork 17 Nil Nil) Nil
            fromList [42,17,23]  `shouldBe` Fork 42 
                                                (Fork 17 Nil Nil)
                                                (Fork 23 Nil Nil) 

