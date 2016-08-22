module Solver where

import qualified Data.Set as Set
import Data.List (findIndex,splitAt)

unsolved = [0,0,0,0,2,1,6,5,7,
            9,6,7,3,4,5,8,0,1,
            2,5,0,8,7,6,4,9,3,
            5,4,8,1,3,2,9,7,6,
            7,2,9,5,6,4,1,3,8,
            1,3,6,7,9,8,0,4,5,
            3,0,2,6,8,9,5,1,4,
            8,1,4,2,5,3,7,6,9,
            6,9,5,4,1,7,3,0,0]::Board

-------------------------------------------------------
-- Solver
-------------------------------------------------------
type Path = [(Board, [Int])]

type Board = [Int]

isFull :: [Int] -> Bool
isFull [] = True
isFull (x:xs) | x == 0 = False
              | otherwise = isFull xs

isLegit :: Board -> Bool
isLegit board = and $ map (legitCell board) [0..80]

legitCell :: Board -> Int -> Bool
legitCell board i = not $ elem (getValue board i) (getGroupValues (indexToCoords i) board)
                 

pathSolve :: Path -> Path
pathSolve (current:p) = pathHelper current p
--pathSolve (current:p) = case current of (_, []) -> error "Right Path"-- pathSolve p
--                                        (b, x) -> let  i = findIndex (==0) b
--                                                  in
--                                                    case i of Nothing -> [(b, [])]
--                                                              (Just val) -> let (new, x') = makeMove b x val
--                                                                           in
--                                                                              pathSolve $ (new,[1..9]) : (b,x') : p

pathHelper :: (Board,[Int]) -> Path -> Path
pathHelper (board,[]) p = pathSolve p
pathHelper (board,moves) p = if i == -1
                             then [(board,[])]
                             else case makeMove board moves i of Nothing -> pathSolve p
                                                                 Just (new,moves') -> pathSolve $ (new,[1..9]) : (board,moves') : p 
  where i = maybe (-1) id (findIndex (==0) board)


makeMove :: Board -> [Int] -> Int -> Maybe (Board, [Int])
makeMove board x i = findLegit (ba, bbs) x i
  where (ba, bb:bbs) = splitAt i board
       

findLegit :: ([Int],[Int]) -> [Int] -> Int -> Maybe (Board, [Int])
findLegit (ba, bb) [] _ = Nothing
findLegit (ba, bb) (x:xs) i = if legitCell nl i then Just (nl, xs) else findLegit (ba, bb) xs i
                                where nl = (ba ++ [x] ++ bb)
                                      

boardSolve :: Board -> Board
boardSolve board = fst $ head (pathSolve [(board, [1..9])])



-------------------------------------------------------
-- Describing
-------------------------------------------------------
indexToCoords :: Int -> (Int, Int)
indexToCoords index = (xCoord index, yCoord index)
  where xCoord index = (mod index 9)
        yCoord index = (div index 9)

coordsToIndex :: (Int, Int) -> Int
coordsToIndex (x, y) = (y*9)+x

getGroupCoords :: (Int, Int) -> [(Int, Int)]
getGroupCoords (x, y) =  (Set.toList (Set.fromList ((colCoords x) ++ (rowCoords y) ++ ninthCoords (x,y))))

getGroupIndices :: Int -> [Int]
getGroupIndices i = (dropValue (coordsToIndex (x,y))) $ map coordsToIndex $ getGroupCoords (x,y)
  where
    (x, y) = indexToCoords i

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

getGroupValues :: (Int, Int) -> [Int] -> [Int]
getGroupValues (x, y) z = map (getValue z) (getGroupIndices (coordsToIndex (x, y)))

getValue :: [Int] -> Int -> Int
getValue v w = v !! w

getConstraints :: (Int, Int) -> [Int] -> [Int]
getConstraints (x, y) z = dropValue 0 (Set.toList (Set.fromList (getGroupValues (x, y) z)))

dropValue :: Int -> [Int] -> [Int]
dropValue x ls | ls == [] = []
               | head ls == x = (dropValue x $ tail ls)
               | head ls /= x = head ls : (dropValue x $ tail ls)

