module Board where

import qualified Data.Vector as V
import Data.List (nub)

type Board = V.Vector Int
type Coord = (Int, Int)
type Index = Int

indexToCoords :: Index -> Coord
indexToCoords index = (xCoord index, yCoord index)
  where xCoord index = (mod index 9)
        yCoord index = (div index 9)

coordsToIndex :: Coord -> Index
coordsToIndex (x, y) = (y*9)+x


getGroupCoords :: Coord -> V.Vector Coord
getGroupCoords (x, y) =  ((V.fromList . nub) ((colCoords x) ++ (rowCoords y) ++ ninthCoords (x,y)))

getGroupIndices :: Index -> V.Vector Index
getGroupIndices i = (dropValue i) $ V.map coordsToIndex $ getGroupCoords (x,y)
  where (x,y) = indexToCoords i
  
colCoords :: Int -> [Coord]
colCoords x = map (colAssist x) [0..8]
  where colAssist x y = (x, y)

rowCoords :: Int -> [Coord]
rowCoords y = map (rowAssit y) [0..8]
  where rowAssit y x = (x, y)

ninthCoords :: Coord -> [Coord]
ninthCoords (x, y) | x <= 2 = ninthAssist [0..2] y
                   | x <= 5 = ninthAssist [3..5] y
                   | otherwise = ninthAssist [6..8] y
  where ninthAssist z y | y <= 2 = comb z [0..2]
                        | y <= 5 = comb z [3..5]
                        | otherwise = comb z [6..8]
          where comb v w =  (,) <$> v <*> w

getGroupValues :: Board -> Index -> V.Vector Index
getGroupValues board i = V.map (getValue board) (getGroupIndices i)

getValue :: Board -> Index -> Int
getValue board i = board V.! i

dropValue :: Int -> V.Vector Int -> V.Vector Int
dropValue x board = V.filter (/=x) board
