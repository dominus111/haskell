--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A2 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (approxSqrt) where

-- Exercise A2
approxSqrt :: Double -> Double -> Double
--approxSqrt d eps = -1
approxSqrt d eps
    | d < 0 = error "Invalid number"
    | eps <= 0 = error "Invalid number"
    | otherwise = approxSqrt' d eps 1

approxSqrt' :: Double -> Double -> Double -> Double
approxSqrt' d eps x | abs((sqrt d) - x) < eps = x
                    | otherwise = approxSqrt' d eps ((x + d/x) / 2)
