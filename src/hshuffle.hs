module Hshuffle (shuffle) where

import System.Random (RandomGen(..), getStdRandom, randomR)
import Data.Map (Map(..),insert,singleton,elems,(!))
import System.IO.Unsafe (unsafePerformIO)

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => [a] -> g -> ([a], g)
fisherYates [] gen = ([], gen)
fisherYates l gen =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)

shuffle :: [a] -> [a]
shuffle l = unsafePerformIO $ getStdRandom $ fisherYates l

uniform :: Double -> Double -> Double
uniform l u = unsafePerformIO $ getStdRandom $ randomR (l,u)

exponential :: Double -> Double
exponential r = expQuant r x
  where expQuant rate val = - (log (1-val))/rate
        x = uniform 0 1
