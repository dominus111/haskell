--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A3 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (longestCommonSubsequence) where

-- Exercise A3
longestCommonSubsequence :: Eq a => [[a]] -> [a]
longestCommonSubsequence [] = []
longestCommonSubsequence [[]] = []
longestCommonSubsequence (xs:[]) = xs
longestCommonSubsequence (xs:ys:xss) = longestCommonSubsequence (lcs xs ys : xss)

longest xs ys = if length xs > length ys then xs else ys

lcs [] ys = []
lcs xs [] = []
lcs (x:xs) (y:ys) 
  | x == y    = x : lcs xs ys
  | otherwise = longest (lcs (x:xs) ys) (lcs xs (y:ys))