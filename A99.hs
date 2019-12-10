{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A9 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (optimalPower,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List
import Data.Ord


data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

-- Exercise A9

optimalPower :: Int -> SMProg
optimalPower 0 = error "Num is 0"
optimalPower 1 = []
optimalPower n | n < 0 = error " Negative number"
               | n ==2 = [Dup, Mul]
               | n `mod` 2 == 0 = optimalPower (n `div` 2) ++ [Dup] ++ [Mul]
               | otherwise = comp2 (some n) (more n)

some n = [Dup] ++ optimalPower (n-1) ++ [Mul] 
more n = comp [optimalPower (fst z) ++ optimalPower (snd z) | z <- getPairs n (factors n)]


factors :: Int -> [Int]
factors n = [ x | x <-[1..n], n `mod` x == 0 ]

getPairs n xs = [ (x,y) | x <- xs, y <- xs , x*y ==n, x<=y]

comp xs = minimumBy (comparing length) xs

comp2 xs ys | length xs >= length ys = ys
            | otherwise = xs

prime :: Int -> Bool
prime n = factors n == [1,n]

primesUpto :: Int -> [Int]
primesUpto n = [ x | x <- [0..n] , prime x ]

greaT n (x:xs) | n `mod` x == 0 = x
               | otherwise = greaT n xs