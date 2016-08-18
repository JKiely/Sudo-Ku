module Builder where

import Data.Char

build :: String -> [Int]
build "" = []
build (x:xs) | x == '.' = 0 : build xs
             | otherwise = (digitToInt x) : (build xs)
