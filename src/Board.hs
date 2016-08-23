module Board where

import qualified Data.Set as Set

type Board = [Int]

indexToCoords :: Int -> (Int, Int)
indexToCoords index = (xCoord index, yCoord index)
  where xCoord index = (mod index 9)
        yCoord index = (div index 9)

coordsToIndex :: (Int, Int) -> Int
coordsToIndex (x, y) = (y*9)+x

getGroupCoords :: (Int, Int) -> [(Int, Int)]
getGroupCoords (x, y) =  (Set.toList (Set.fromList ((colCoords x) ++ (rowCoords y) ++ ninthCoords (x,y))))

getGroupIndices :: Int -> [Int]
getGroupIndices i = (dropValue i) $ map coordsToIndex $ getGroupCoords (x,y)
  where (x,y) = indexToCoords i
  
colCoords :: Int -> [(Int, Int)]
colCoords x = map (colAssist x) [0..8]
  where colAssist x y = (x, y)

rowCoords :: Int -> [(Int, Int)]
rowCoords y = map (rowAssit y) [0..8]
  where rowAssit y x = (x, y)

ninthCoords :: (Int, Int) -> [(Int, Int)]
ninthCoords (x, y) | x <= 2 = ninthAssist [0..2] y
                   | x <= 5 = ninthAssist [3..5] y
                   | otherwise = ninthAssist [6..8] y
  where ninthAssist z y | y <= 2 = comb z [0..2]
                        | y <= 5 = comb z [3..5]
                        | otherwise = comb z [6..8]
          where comb v w =  (,) <$> v <*> w

getGroupValues :: Board -> Int -> [Int]
getGroupValues board i = map (getValue board) (getGroupIndices i)

getValue :: Board -> Int -> Int
getValue board i = board !! i

dropValue :: Int -> [Int] -> [Int]
dropValue x board | board == [] = []
                  | head board == x = (dropValue x (tail board))
                  | head board /= x = head board : (dropValue x (tail board))
