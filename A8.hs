{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A8 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (findMaxReducers,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Control.Monad
import Data.Ord
import Data.List



data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction]

instance NFData (Instruction)

-- Exercise A8

findMaxReducers :: Stack -> [SMProg]
findMaxReducers [] = []
findMaxReducers [x] = [[]]
findMaxReducers s =  ans $ getM ( getAllP 1 $ length s) s

getAllP m l | (m < l) = replicateM m [Add, Mul, Pop] ++ getAllP (m+1) l
            | otherwise = []

getM :: [SMProg] -> Stack -> [(Int, SMProg)]
getM [] s = []
getM (x:[]) s = [(last (evalInst s x), x)]
getM (x:y:xs) s | length x1 == 1 && length y1 == 1 && head x1 > head y1 = (head x1, x) : getM (xs) s
                | length x1 == 1 && length y1 == 1 && head x1 == head y1 = (head x1, x) : getM (y:xs) s
                | otherwise = getM (y:xs) s
 where x1 = evalInst s x
       y1 = evalInst s y





ans [] =[]
ans xs = [snd (x) | x <- xs, fst (x) == sol]
 where sol = fst (last (sortBy (comparing fst) xs))

evalInst :: Stack -> SMProg -> Stack
evalInst [] _ = error "Empty Stack"
evalInst xs [] = xs
evalInst s (i:is)   | i == Add = evalInst (sAdd s) is
                    | i == Mul = evalInst (sMul s) is
                    | i == Dup = evalInst (sDup s) is
                    | i == Pop = evalInst (sPop s) is


sAdd :: Stack -> Stack
sAdd [] = []
sAdd [s] = error "Not enough elements"
sAdd (s1:s2:ss) = (s1+s2):ss

sMul :: Stack -> Stack
sMul [] = []
sMul [s] = error "Not enough element"
sMul (s1:s2:ss) = (s1*s2):ss

sDup :: Stack -> Stack
sDup [] = []
sDup [s] = s:s:[]
sDup (s:ss) = s:s:ss

sPop :: Stack -> Stack
sPop [] = []
sPop [s] = []
sPop (s:ss) = ss