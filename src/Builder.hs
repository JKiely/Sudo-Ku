module Builder where

import Data.Vector (Vector(..), cons, fromList)
import Data.Char

build :: String -> Vector Int
build "" = fromList []
build (x:xs) | x == '.' = 0 `cons` build xs
             | otherwise = (digitToInt x) `cons` (build xs)
