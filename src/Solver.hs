module Solver where

import Board

import Data.List (findIndex,splitAt)


type Path = [(Board, [Int])]

-- Returns if a board is a legitimat solved board
isLegit :: Board -> Bool
isLegit board = and $ map (legitCell board) [0..80]

-- Returns if a cell is currently not clashing with neighbors
legitCell :: Board -> Int -> Bool
legitCell board i = not $ elem (getValue board i) (getGroupValues i board)
                 
-- Breaks up arguments and passes them on to pathHelper
pathSolve :: Path -> Path
pathSolve (current:p) = pathHelper current p

-- If a board has no legitimate next moves it is dropped
-- However if it does then it calls makeMove to make a move and appends that move to the path
pathHelper :: (Board,[Int]) -> Path -> Path
pathHelper (board,[]) p = pathSolve p
pathHelper (board,moves) p = if i == -1
                             then [(board,[])]
                             else case makeMove board moves i of Nothing -> pathSolve p
                                                                 Just (new,moves') -> pathSolve $ (new,[1..9]) : (board,moves') : p 
  where i = maybe (-1) id (findIndex (==0) board)

-- Splits the board on the index and passes the parts to find legit
makeMove :: Board -> [Int] -> Int -> Maybe (Board, [Int])
makeMove board x i = findLegit (ba, bbs) x i
  where (ba, bb:bbs) = splitAt i board
       
-- Takes a split board and returns a board with a legit move, or nothing
findLegit :: ([Int],[Int]) -> [Int] -> Int -> Maybe (Board, [Int])
findLegit (ba, bb) [] _ = Nothing
findLegit (ba, bb) (x:xs) i = if legitCell nl i then Just (nl, xs) else findLegit (ba, bb) xs i
                                where nl = (ba ++ [x] ++ bb)

-- the main fuction, call the pathSolve function and returns the board
boardSolve :: Board -> Board
boardSolve board = fst $ head (pathSolve [(board, [1..9])])

