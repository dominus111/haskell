--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A4 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (neighbours) where

import Data.List 
import Data.Ord

type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double

metric :: Num a => (Point a) -> (Point a) -> a
metric (a,b) (c,d) = abs(a-c) + abs(b-d)

-- Exercise A4

neighbours :: Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k d p xs
    | k < 0 = error "Negative k"
    | otherwise = neighbours' k d p xs


neighbours' k d p xs = take k (allNeighbours d p xs)


allNeighbours d p xs = map snd (distPair d p xs)

distPair d p xs = sortBy (comparing fst) (pairs d p xs)

pairs d p xs = zip [d p x | x <- xs] xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs)  = quickSort smaller ++ [x] ++ quickSort larger
    where smaller = filter (<=x) xs
          larger  = filter (> x) xs