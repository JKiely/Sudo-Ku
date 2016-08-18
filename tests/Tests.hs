module Main where

import Test.Hspec

import Builder
import Solver

main :: IO ()
main = hspec $ do
  describe "Builder" $ do
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

  describe "Solver" $ do
    it "checks if a board is full" $ do
      (isFull [2,3,4,5]) `shouldBe` True
    it "knows if board isn't full" $ do
      (isFull [2,0,4,5]) `shouldBe` False
    it "can get coords" $ do
      (indexToCoords 71) `shouldBe` (8,7)
    it "can get an index from coords" $ do
      (coordsToIndex (8, 7)) `shouldBe` 71
    it "can get the coords of a col based on one coord" $ do
      let cCoords = [(8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8)]
      (colCoords 8) `shouldBe` cCoords
    it "can get the coords of a row based on one coord" $ do
      let rCoords = [(0,7),(1,7),(2,7),(3,7),(4,7),(5,7),(6,7),(7,7),(8,7)]
      (rowCoords 7) `shouldBe` rCoords
    it "can get ninth coords" $ do
      let nCoords = [(6,6),(6,7),(6,8),(7,6),(7,7),(7,8),(8,6),(8,7),(8,8)]
      (ninthCoords (8, 7)) `shouldBe` nCoords

    
