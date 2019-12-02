--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A5 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

module Exercises (findBonding) where
import Data.List

-- Exercise A5
gr x y = x+y <45
od x y = odd (x+y)

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
       uni = pairs p xs

--pairs :: (b -> b -> Bool) -> [b] -> [(b, b)]
pairs p [] = []
pairs p (x:xs) = [(x,y)| y <- xs, p x y == True ] ++ pairs p xs


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