-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2019
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return a randome value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (alphaNorm, countAllReds, printLambda, parseLet, letToLambda,
    LamExpr(LamApp, LamAbs, LamVar), LetExpr(LetApp, LetDef, LetFun, LetVar, LetNum),
    lambdaToLet) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Data.List
import Parsing
--import qualified Data.List as L
--import qualified Data.Map  as M

-- abstract data type for simple lambda calculus expressions
data LamExpr = LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int deriving (Show, Eq)

-- abstract data type for simple let expressions
data LetExpr = LetApp LetExpr LetExpr  |  LetDef [([Int], LetExpr)] LetExpr |  LetFun Int | LetVar Int | LetNum Int deriving (Show, Eq)
-- END OF CODE YOU MUST NOT MODIFY

--allFreeVar :: VarM
-- ADD YOUR OWN CODE HERE
-- Challenge 1
-- generate the alpha normal form for a simple lambda calculus expression
-- each bound variable is chosen to be the first one possible
--type Name = Int

--type VarMap = M.Map Int Int

alphaNorm :: LamExpr -> LamExpr
alphaNorm x =  convert'' (allFreeVar x) x

allFreeVar exp = [(x,x) | x <- getFreeVar exp]

getFreeVar :: LamExpr -> [Int]
getFreeVar (LamVar x) = [x]
getFreeVar (LamAbs x e) = delete x (getFreeVar e)
getFreeVar (LamApp e1 e2) = nub $ getFreeVar e1 ++ getFreeVar e2

convert'' :: [(Int, Int)] -> LamExpr -> LamExpr
convert'' dependent (LamVar x) = LamVar $ findIn x dependent
convert'' dependent (LamApp e1 e2) = LamApp (convert'' dependent e1) (convert'' dependent e2)
convert'' dependent (LamAbs x e) = LamAbs newName $ convert'' newDep e
 where freeVars  = delete x $ getFreeVar e
       taken = findUsed freeVars dependent
       newName = head [z | z <- [0..], z `notElem` taken]
       newDep = (x, newName): dependent

findIn :: Int -> [(Int,Int)] -> Int
findIn x (d:dx) | fst d == x = snd d
                | otherwise = findIn x dx

findUsed :: [Int] -> [(Int,Int)] -> [Int]
findUsed _ [] = []
findUsed [] _ = []
findUsed (x:xs) (d:dx) = (findIn x (d:dx)): findUsed xs (d:dx)




-- Challenge 2
-- count all reduction paths for a given lambda expression m, of length up to a given limit l
--countAllReds (LamApp (LamAbs 0 (LamVar 0)) (LamAbs 1 (LamVar 1))) 1 == 0,
--countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamVar 3))(LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))) 2
countAllReds :: LamExpr -> Int -> Int
countAllReds _ 0 = 0
countAllReds x limit = countRedu x limit 0

countRedu :: LamExpr -> Int -> Int -> Int
countRedu x limit count | rRight x == rLeft x = rigthRedu (x) (limit -1) count
                     | otherwise = (rigthRedu x (limit -1) count) + (leftRedu x (limit -1) count)

rigthRedu x limit count | limit == 0 && finished (rRight) x = 1
                        | limit == 0 && not (finished (rRight) x) = 0
                        | otherwise = countRedu (rRight x ) limit count

leftRedu x limit count | limit == 0 && finished (rLeft) x = 1
                       | limit == 0 && not (finished (rLeft) x) = 0
                       | otherwise = countRedu (rLeft x) limit count

finished :: (LamExpr -> LamExpr) -> LamExpr -> Bool
finished f e 
    | e == e2 = True
    | otherwise = False
    where e2 = (f e)


rLeft :: LamExpr -> LamExpr
rLeft (LamVar x) = LamVar x
rLeft (LamAbs x e) = LamAbs x (rLeft e)
rLeft (LamApp e1@(LamAbs x e) e2) | e1 == e3 = subst e x e2
                                  | otherwise = LamApp e3 e2
                                    where e3 = rLeft e1
rLeft (LamApp e1 e2) | e1 == e3 = LamApp e1 (rLeft e2)
                     | otherwise = LamApp e3 e2
                      where e3 = rLeft e1

-- rigth beta reduction
rRight :: LamExpr -> LamExpr
rRight (LamVar x) = LamVar x
rRight (LamAbs x e) = LamAbs x (rRight e)
rRight (LamApp e1@(LamAbs x e) e2) | e2 == e3 = subst e x e2
                                   | otherwise = LamApp e1 e3
                                    where e3 = rRight e2
rRight (LamApp e1 e2) | e2 == e3 = LamApp (rRight e1) e2
                      | otherwise = LamApp e1 e3
                       where e3 = rRight e2

subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e | x /= y && not (free x e) = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e | x /=y && (free x e) = let x' = rename x in
                                                subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e | x == y = subst e1 x e
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e) 

free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

rename :: Int -> Int
rename x = (40 * x) + 1
-- Challenge 3 
-- pretty print a lambda expression, combining abstraction variables
-- also recognising Scott numerals and printing these as numbers
-- finalising omitting brackets where possible and safe to do so
printLambda :: LamExpr -> String
printLambda _ = ""


-- Challenge 4
-- parse recursive let expression, possibly containing numerals
parseLet :: String -> Maybe LetExpr
parseLet _ = Just (LetVar (-1))


-- Challenge 5
-- translate a let expression into lambda calculus, using Scott numerals
-- convert let symbols to lambda variables using Jansen's techniques rather than Y
letToLambda :: LetExpr -> LamExpr
letToLambda _ = LamVar (-1)


-- Challenge 6
-- convert a lambda calculus expression into one using let expressions and application
-- can use lambda lifting techniques described in wikipedia article
lambdaToLet :: LamExpr -> LetExpr
lambdaToLet _ = LetVar (-1)