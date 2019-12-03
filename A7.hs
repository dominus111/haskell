{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A7 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (evalInst,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

-- Exercise A7
evalInst :: Stack -> SMProg -> Stack
evalInst [] _ = error "Empty Stack"
evalInst xs [] = xs
evalInst s (i:is)   | p == Add = evalInst (sAdd s) is
                    | p == Mul = evalInst (sMul s) is
                    | p == Dup = evalInst (sDup s) is
                    | p == Pop = evalInst (sPop s) is


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