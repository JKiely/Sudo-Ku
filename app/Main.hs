module Main where

import Builder (build)
import Solver (solve)

board =  (build "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")

sudoku = (solve board)

main :: IO ()
main = print sudoku
