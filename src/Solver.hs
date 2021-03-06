module Solver where

import qualified Data.Vector as V
import Board
import Hshuffle
import Data.List (findIndex,splitAt)

type Branch = (Board, [Int])
type Path = [Branch]

--Wrapper function, calls whatever solve function we're using right now.
solve :: Board -> Board
solve board = (boardSolve board)

-- Returns if a board is a legitimat solved board
isLegit :: Board -> Bool
isLegit board = not (0 `elem` board) && (and $ map (legitCell board) [0..80])

-- Returns if a cell is currently not clashing with neighbors
legitCell :: Board -> Index -> Bool
legitCell board i = not $ elem (value) (getGroupValues board i)
  where value = getValue board i
                 
-- Breaks up arguments and passes them on to pathHelper
pathSolve :: Path -> Path
pathSolve (current:p) = pathHelper current p

-- If a board has no legitimate next moves it is dropped
-- However if it does then it is passed to path findEmpty
pathHelper :: Branch -> Path -> Path
pathHelper (board, []) p = pathSolve p
pathHelper (board, moves) p = findEmpty (board, moves) p

-- Looks for an empty square, and passes it on to addMove if it finds it
findEmpty :: Branch -> Path -> Path
findEmpty (board, moves) p = case index of Nothing ->
                                             [(board,[])]
                                           Just index ->
                                             (addMove board moves p index)
  where index = (V.findIndex (==0) board)

-- Tries to make a move and add it to the path
addMove :: Board -> [Int] -> Path -> Index -> Path
addMove board moves p i = case move of Nothing ->
                                         pathSolve p
                                       Just (new,moves') ->
                                         pathSolve $ (new, shuffle [1..9]) : (board,moves') : p
  where move = makeMove board moves i

-- Splits the board on the index and passes the parts to find legit
makeMove :: Board -> [Int] -> Index -> Maybe (Board, [Int])
makeMove board moves i = findLegit (rightBoard, V.tail leftBoard) moves i
  where (rightBoard, leftBoard) = V.splitAt i board
       
-- Takes a split board and returns a board with a legit move, or nothing
findLegit :: (V.Vector Int, V.Vector Int) -> [Int] -> Index -> Maybe (Board, [Int])
findLegit (rightBoard, leftBoard) [] _ = Nothing
findLegit (rightBoard, leftBoard) (move:moves) i = if (legitCell newBoard i)
                                                   then Just (newBoard, moves) else findLegit (rightBoard, leftBoard) moves i
                                where newBoard = (rightBoard V.++ (V.fromList [move]) V.++ leftBoard)

-- the main fuction, call the pathSolve function and returns the board
boardSolve :: Board -> Board
boardSolve board = fst $ head (pathSolve [(board, shuffle [1..9])])
