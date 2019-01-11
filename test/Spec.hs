import Test.Hspec
import Test.QuickCheck

import Lib

genHeight :: Gen Int
genHeight = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 1)

listOfHeight :: Gen [Int]
listOfHeight = listOf genHeight

main :: IO ()
main = hspec $ do
  describe "largest rectangle" $ do
    it "returns size of the largest rectangle" $ do
      largestRectangle [1,2,3,4,5] `shouldBe` 9
      largestRectangle [5,4,3,2,1] `shouldBe` 9
      largestRectangle [4,4,23,21,13,20,22,6,8,8,4,16,14,6] `shouldBe` 65
      largestRectangle [6,14,16,4,8,8,6,22,20,13,21,23,4,4] `shouldBe` 65

    it "result is equal to the naive solution" $ do
      forAll listOfHeight $ \xs -> largestRectangle xs == naiveSolution xs

    it "result should be the same if list is reversed" $ do
      forAll listOfHeight $ \xs -> largestRectangle xs == largestRectangle (reverse xs)