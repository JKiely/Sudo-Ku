module Main where

import Test.Hspec
import Data.Vector (Vector(..), fromList)

import Builder
import Board
import Solver

main :: IO ()
main = hspec $ do
  describe "Builder" $ do
    it "makes a board" $ do
      let inputString = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
      let outputArray = fromList [4, 0, 0, 0, 0, 0, 8, 0, 5,
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

  describe "Board" $ do
    let board = fromList [4, 0, 0, 0, 0, 0, 8, 0, 5,
                 0, 3, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 7, 0, 0, 0, 0, 0,
                 0, 2, 0, 0, 0, 0, 0, 6, 0,
                 0, 0, 0, 0, 8, 0, 4, 0, 0,
                 0, 0, 0, 0, 1, 0, 0, 0, 0,
                 0, 0, 0, 6, 0, 3, 0, 7, 0,
                 5, 0, 0, 2, 0, 0, 0, 0, 0,
                 1, 0, 4, 0, 0, 0, 0, 0, 0]
      
    it "can get coords" $ do
      (indexToCoords 71) `shouldBe` (8,7)
      
    it "can get an index from coords" $ do
      (coordsToIndex (8, 7)) `shouldBe` 71
      
    it "can get the coords of a col based on one coord" $ do -- Refactor
      let cCoords = [(8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8)]
      
      (colCoords 8) `shouldBe` cCoords
    it "can get the coords of a row based on one coord" $ do -- Refactor
      let rCoords = [(0,7),(1,7),(2,7),(3,7),(4,7),(5,7),(6,7),(7,7),(8,7)]
      
      (rowCoords 7) `shouldBe` rCoords
    it "can get ninth coords" $ do -- Refactor 
      let nCoords = [(6,6),(6,7),(6,8),(7,6),(7,7),(7,8),(8,6),(8,7),(8,8)]
      
      (ninthCoords (8, 7)) `shouldBe` nCoords
    it "can get group coords" $ do -- Refactor, redundant?
      let gCoords = fromList [(8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8),(0,7),(1,7),(2,7),(3,7),(4,7),(5,7),(6,7),(7,7),(6,6),(6,8),(7,6),(7,8)]
      (getGroupCoords (8, 7)) `shouldBe` gCoords
      
    it "can get group indices" $ do
      let gIndices = fromList [8,17,26,35,44,53,62,80,63,64,65,66,67,68,69,70,60,78,61,79]
      (getGroupIndices 71) `shouldBe` gIndices
      
    it "can get group values" $ do
      let gValues = fromList [5,0,0,0,0,0,0,0,5,0,0,2,0,0,0,0,0,0,7,0]
      (getGroupValues board 71) `shouldBe` gValues
      
    it "can drop values from a list" $ do
      let l =  fromList [5,0,0,2,0,0,0,0,0,7,0,0,5,0,0,0,0,0,0,0,0]
      dropValue 0 l `shouldBe` (fromList [5,2,7,5])
      

  describe "Solver" $ do
    let complete = fromList [4,8,3,9,2,1,6,5,7,
                    9,6,7,3,4,5,8,2,1,
                    2,5,1,8,7,6,4,9,3,
                    5,4,8,1,3,2,9,7,6,
                    7,2,9,5,6,4,1,3,8,
                    1,3,6,7,9,8,2,4,5,
                    3,7,2,6,8,9,5,1,4,
                    8,1,4,2,5,3,7,6,9,
                    6,9,5,4,1,7,3,8,2]
                   
        wrong =  fromList [4,8,3,9,2,1,6,5,7,
                  9,6,7,3,4,5,8,2,1,
                  2,5,1,8,7,6,4,9,3,
                  5,4,8,1,3,2,9,7,6,
                  7,2,9,5,6,4,1,3,8,
                  1,3,6,7,9,8,2,4,5,
                  3,7,2,6,8,9,5,1,4,
                  8,1,4,2,5,3,7,6,9,
                  6,9,5,4,1,7,3,8,9]

        oneoff = fromList [0,8,3,9,2,1,6,5,7,
                  9,6,7,3,4,5,8,2,1,
                  2,5,1,8,7,6,4,9,3,
                  5,4,8,1,3,2,9,7,6,
                  7,2,9,5,6,4,1,3,8,
                  1,3,6,7,9,8,2,4,5,
                  3,7,2,6,8,9,5,1,4,
                  8,1,4,2,5,3,7,6,9,
                  6,9,5,4,1,7,3,8,2]
                 
        severaloff = fromList [0,0,0,0,2,1,6,5,7,
                      9,6,7,3,4,5,8,0,1,
                      2,5,0,8,7,6,4,9,3,
                      5,4,8,1,3,2,9,7,6,
                      7,2,9,5,6,4,1,3,8,
                      1,3,6,7,0,8,0,4,5,
                      3,0,2,6,8,9,5,1,4,
                      8,1,4,2,5,3,7,6,9,
                      6,9,5,4,1,7,3,0,0]
                    
        solved1 = fromList [4,8,3,9,2,1,6,5,7,
                  9,6,7,3,4,5,8,2,1,
                  2,5,1,8,7,6,4,9,3,
                  5,4,8,1,3,2,9,7,6,
                  7,2,9,5,6,4,1,3,8,
                  1,3,6,7,9,8,2,4,5,
                  3,7,2,6,8,9,5,1,4,
                  8,1,4,2,5,3,7,6,9,
                  6,9,5,4,1,7,3,8,2]

        normal = fromList [0, 0, 3, 0, 2, 0, 6, 0, 0,
                  9, 0, 0, 3, 0, 5, 0, 0, 1,
                  0, 0, 1, 8, 0, 6, 4, 0, 0,
                  0, 0, 8, 1, 0, 2, 9, 0, 0,
                  7, 0, 0, 0, 0, 0, 0, 0, 8,
                  0, 0, 6, 7, 0, 8, 2, 0, 0,
                  0, 0, 2, 6, 0, 9, 5, 0, 0,
                  8, 0, 0, 2, 0, 3, 0, 0, 9,
                  0, 0, 5, 0, 1, 0, 3, 0, 0]
                 
        solved2 = fromList [4, 8, 3, 9, 2, 1, 6, 5, 7,
                   9, 6, 7, 3, 4, 5, 8, 2, 1,
                   2, 5, 1, 8, 7, 6, 4, 9, 3,
                   5, 4, 8, 1, 3, 2, 9, 7, 6,
                   7, 2, 9, 5, 6, 4, 1, 3, 8,
                   1, 3, 6, 7, 9, 8, 2, 4, 5,
                   3, 7, 2, 6, 8, 9, 5, 1, 4,
                   8, 1, 4, 2, 5, 3, 7, 6, 9,
                   6, 9, 5, 4, 1, 7, 3, 8, 2]


    it "knows if a solution is legit" $ do
      (isLegit complete) `shouldBe` True
      
    it "knows if a solution is wrong" $ do
      (isLegit wrong) `shouldBe` False

    it "knows an incomplete board is wrong" $ do
      (isLegit oneoff) `shouldBe` False
      
    it "Knows if a cell is wrong" $ do
      (legitCell wrong 80) `shouldBe` False
      
    it "Can actually solve a sudoku" $ do
      (boardSolve oneoff) `shouldBe` solved1
        
    it "Can actually solve a slightly harder sudoku" $ do
      (boardSolve severaloff) `shouldBe` solved1
        
    it "Can solve a normal sudoku" $ do
      (boardSolve normal) `shouldBe` solved2

--    it "Can solve a hard sudoku" $ do
--      let hard = [4, 0, 0, 0, 0, 0, 8, 0, 5, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 8, 0, 4, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 6, 0, 3, 0, 7, 0, 5, 0, 0, 2, 0, 0, 0, 0, 0, 1, 0, 4, 0, 0, 0, 0, 0, 0]
--          solved = (boardSolve hard)
--      (isLegit solved) `shouldBe` True
