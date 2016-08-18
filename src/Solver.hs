module Solver where

isFull :: [Int] -> Bool
isFull [] = True
isFull (x:xs) | x == 0 = False
              | otherwise = isFull xs

indexToCoords :: Int -> (Int, Int)
indexToCoords index = (xCoord index, yCoord index)
  where xCoord index = (mod index 9)
        yCoord index = (div index 9)
