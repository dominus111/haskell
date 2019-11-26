--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A1 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (histogram) where

-- Exercise A1
histogram :: Int -> [Int] -> [Int]
histogram n [] = error "Empty List"
histogram n xs | n <= 0  = error "Invalid input"
               |otherwise = [pairLength m xs | m <- hisRange n xs]

pairLength m xs =  length[x| x <-xs, (fst m) <= x , x <= (snd m)]
hisRange n xs = zip [0,n .. k*n] [n-1, 2*n-1 .. (n*(k+1)-1)]
 where k = maximum(xs)`div`n