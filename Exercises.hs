{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR COURSEWORK 1 for COMP2209, 2019
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2019

module Exercises (histogram,approxSqrt,longestCommonSubsequence,neighbours,findBonding,insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..),Instruction(..),Stack,SMProg,evalInst,findMaxReducers,optimalPower) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List
import Data.Ord
import Control.Monad

-- Exercise A1
histogram :: Int -> [Int] -> [Int]
histogram n [] = error "Empty List"
histogram n xs | n <= 0  = error "Invalid input"
               |otherwise = [pairLength m xs | m <- hisRange n xs]

pairLength m xs =  length[x| x <-xs, (fst m) <= x , x <= (snd m)]
hisRange n xs = zip [0,n .. k*n] [n-1, 2*n-1 .. (n*(k+1)-1)]
 where k = maximum(xs)`div`n

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

-- Exercise A4
type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double

neighbours :: Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k d p xs
    | k < 0 = error "Negative k"
    | otherwise = neighbours' k d p xs


neighbours' k d p xs = take k (allNeighbours d p xs)


allNeighbours d p xs = map snd (distPair d p xs)

distPair d p xs = sortBy (comparing fst) (pairs d p xs)

pairs d p xs = zip [d p x | x <- xs] xs

-- Exercise A5
findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding p xs | xs == [] = Just []
                 | p (head xs) (head xs) /= True || False = Nothing
                 | length (arr) == 0 = Nothing
                 | odd(length xs) = Nothing
                 | otherwise = Just arr
        where arr = bonding p xs


--bonding :: Eq a => (a -> a -> Bool) -> [(a,a)] -> [(a,a)]
bonding p xs =  findValid (cartProduct (makeLists (pairP uni) uni)) zz
 where zz = length (xs) `div` 2
       uni = pairs' p xs

--pairs :: (b -> b -> Bool) -> [b] -> [(b, b)]
pairs' p [] = []
pairs' p (x:xs) = [(x,y)| y <- xs, p x y == True ] ++ pairs' p xs


pairP xs = nub [a | (a,b) <- xs]

makeLists [] ys = []
makeLists (x:xs) ys = [ (a,b) | (a,b) <- ys, a == x] : makeLists xs ys

cartProduct :: [[a]] -> [[a]]
cartProduct = foldr
    (\xs as ->
        [ x : a
        | x <- xs
        , a <- as ])
    [[]]

findValid (x:xs) num | (valid x num) = printVal x
                     | otherwise = findValid xs num

valid (x:xs) num | length (getAnswer (x:xs)) == num = True
                 | otherwise = False


printVal xs = inverse (getAnswer xs)

getAnswer :: Eq a =>  [(a,a)] -> [(a,a)]
getAnswer [] = []
getAnswer xs = head(xs) : getAnswer (gets (head xs) xs)

gets z xs = [(x,y) |(x,y) <- xs, x /= (fst z), y /= (snd z), x /= (snd z), y /= (fst z)]

inverse :: Eq a =>  [(a,a)] -> [(a,a)]
inverse [] = []
inverse ((a,b):xs) = (a,b):(b,a):inverse xs

-- Exercise A6

data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode v (Leaf, []) = putIn v (Leaf, [])
insertFromCurrentNode v zi = (compare' v zi)


putIn v zi = attach (Node (Leaf) v 1 (Leaf)) zi

attach :: VTree a -> Zipper a -> Zipper a  
attach t (_, bs) = (t, bs) 


getVal ((Node l x i t), val) = x

compare' v zi | v == getVal zi = zi
              | v < getVal zi && canUp zi && v > getVal up  = putT v up
              | v > getVal zi && canUp zi && v < getVal up  = putT v up
              | canUp zi = compare' v up
              | otherwise = putT v (zi)
 where up = goUp zi

putT v zi | (getNode left /= Leaf) && v < getVal zi = putT v left
            | (getNode right /= Leaf) && v > getVal zi = putT v right
            | (getNode left == Leaf) && v < getVal zi = (putIn v left)
            | otherwise = putIn v right
 where left = goLeft zi
       right = goRight zi


getNode (t, tr) = t

canUp :: Zipper a -> Bool
canUp ( t , [] ) = False
canUp ( t , tr ) = True

modify :: Zipper a -> Zipper a
modify (Node l x i r, ts) = (Node l x (i+1) r, ts)
modify (Leaf, ts) = (Leaf, ts) 

goLeft,goRight,goUp :: Zipper a -> Zipper a
goLeft (Node l x i r , ts) = modify (l , L x (i) r:ts)
goRight (Node l x i r , ts) = modify (r , R x (i) l:ts)
goUp (t , L x i r : ts) = (Node t x (i+1) r , ts)
goUp (t , R x i l : ts) = (Node l x (i+1) t , ts)

mkTree :: Ord a => [a] -> Zipper a
mkTree = foldl (\z -> \x -> insertFromCurrentNode x z) (Leaf,[])

-- Exercise A7

data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

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


-- Exercise A9

optimalPower :: Int -> SMProg
optimalPower 0 = error "Num is 0"
optimalPower 1 = []
optimalPower n | n < 0 = error " Negative number"
               | n ==2 = [Dup, Mul]
               | n `mod` 2 == 0 = optimalPower (n `div` 2) ++ [Dup] ++ [Mul]
               | not(prime n) = comp (oddy n) (some n)
               | otherwise = [Dup] ++ optimalPower (n-1) ++ [Mul] 

oddy n = optimalPower (n `div `greaT n (primesUpto n)) ++ optimalPower (greaT n (primesUpto n))
some n = [Dup] ++ optimalPower (n-1) ++ [Mul] 

comp xs ys | length xs >= length ys = ys
           | otherwise = xs

factors :: Int -> [Int]
factors n = [ x | x <-[1..n], n `mod` x == 0 ]

prime :: Int -> Bool
prime n = factors n == [1,n]

primesUpto :: Int -> [Int]
primesUpto n = [ x | x <- [0..n] , prime x ]

greaT n (x:xs) | n `mod` x == 0 = x
               | otherwise = greaT n xs