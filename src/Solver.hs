module Solver where

import Board

import Data.List (findIndex,splitAt)


type Path = [(Board, [Int])]


isFull :: [Int] -> Bool
isFull [] = True
isFull (x:xs) | x == 0 = False
              | otherwise = isFull xs

isLegit :: Board -> Bool
isLegit board = and $ map (legitCell board) [0..80]

legitCell :: Board -> Int -> Bool
legitCell board i = not $ elem (getValue board i) (getGroupValues i board)
                 

pathSolve :: Path -> Path
pathSolve (current:p) = pathHelper current p

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

