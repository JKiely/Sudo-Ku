module Main where

import Builder
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Verify that builder makes a board" $ do
    it "makes a board" $ do
      let inputString = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
      let outputArray = [4, 0, 0, 0, 0, 0, 8, 0, 5,
                     0, 3, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 7, 0, 0, 0, 0, 0,
                     0, 2, 0, 0, 0, 0, 0, 6, 0,
                     0, 0, 0, 0, 8, 0, 4, 0, 0,
                     0, 0, 0, 0, 1, 0, 0, 0, 0,
                     0, 0, 0, 6, 0, 3, 0, 7, 0,
                     5, 0, 0, 2, 0, 0, 0, 0, 0,
                     1, 0, 4, 0, 0, 0, 0, 0, 0]
                    
      let grid = build inputString
      grid `shouldBe`  outputArray
