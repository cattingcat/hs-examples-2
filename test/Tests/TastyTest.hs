module Tests.TastyTest (main) where
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  specs1 <- testSpecs arithmeticSpec
  specs2 <- testSpecs stringSpec
  defaultMain (testGroup "(no tests)" (specs1 ++ specs2))


arithmeticSpec :: Spec
arithmeticSpec =
  describe "Arithmetic" $ do
    it "Sum" $
      2 + 2 `shouldBe` 4
    it "Product" $
      2 * 2 `shouldBe` 4

stringSpec :: Spec
stringSpec =
  describe "String" $ do
    it "concat" $
      "2" ++ "2" `shouldBe` "22"
