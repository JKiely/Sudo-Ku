module Solver where

isFull :: [Int] -> Bool
isFull [] = True
isFull (x:xs) | x == 0 = False
              | otherwise = isFull xs

indexToCoords :: Int -> (Int, Int)
indexToCoords index = (xCoord index, yCoord index)
  where xCoord index = (mod index 9)
        yCoord index = (div index 9)

coordsToIndex :: (Int, Int) -> Int
coordsToIndex (x, y) = (y*9)+x

--getGroupCoords :: (Int, Int) -> [(Int, Int)]
--getGroupCoords (x, y) = (rowCoords x) : (colCoords y) : ninthCoords (x,y)

colCoords :: Int -> [(Int, Int)]
colCoords x = map (colAssist x) [0..8]
  where colAssist x y = (x, y)

rowCoords :: Int -> [(Int, Int)]
rowCoords y = map (rowAssit y) [0..8]
  where rowAssit y x = (x, y)
