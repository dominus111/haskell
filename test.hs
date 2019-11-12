import Data.Char

double 10 = 10 + 10
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns
add' :: Int -> (Int -> Int)
add' x y = x+y
add :: Num a => a -> a -> a
add x y = x+y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z

quadroots :: Float -> Float -> Float -> String
quadroots a b c | a == 0 = error "Non quadratic"
                | b*b - 4*a*c == 0 = "Root is " ++ show(b/2*a)
                | otherwise = "Upper root is "
                               ++ show((-b + sqrt(abs(b*b - 4*a*c)))/ 2*a)
                               ++ " and Lower root is "
                               ++ show((-b - sqrt(abs(b*b - 4*a*c)))/ 2*a)


sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

productList :: [Int] -> Int
productList [] = 1
productList (x:xs) = x*productList xs

quicktros [] = []
quicktros (x:xs) = quicktros ls ++ [x] ++ quicktros rs
                   where 
                     ls = [ a | a <- xs , a > x ]
                     rs = [ a | a <- xs , a < x ]

n = a `div` length xs
    where
     a = 10 
     xs = [1,2,3,4,5]

bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1,2], [3,4]]

add1 :: Int ->Int ->Int ->Int
add1 a b c = a+b+c

copy :: a -> (a,a)
copy a = (a,a)

positions x xs = [ index | (x',index) <- zip xs [0..], x==x' ]

count :: Char -> String -> Int
count x xs = length [ x' | x' <- xs , x==x'] 



re :: [a] -> [a]
re [] = []
re (x:xs) = re xs ++ [x]


init' (x:xs) | null xs = []
             | otherwise = x : init' xs


fac' :: Int -> Int -> Int
fac' 0 acc = acc
fac' n acc = fac' (n-1) (n*acc)

splitAt' :: Int -> [a] -> ( [a] , [a] )
splitAt' n xs = (take n xs, drop n xs)

last' [] = error "Empty list"
last' (x:[]) = x
last' (x:xs) = last xs

fetch 1 [] = error "Empty List"
fetch n [] = error "Empty List1" 
                    ++ show(n)
fetch 1 (x:_) = x
fetch n (_:xs) = fetch (n-1) xs

toUpperStr :: String -> String
toUpperStr [ ] = [ ]
toUpperStr (c:cs) = toUpper c : toUpperStr cs 

classify age = case age of 0 -> "newborn"
                           1 -> "infant"
                           2 -> "toddler"
                           _ -> "senior citizen"

secsToWeeks secs = let perMinute = 60
                       perHour   = 60 * perMinute
                       perDay    = 24 * perHour
                       perWeek   =  7 * perDay
                   in  secs / perWeek

add'' = const 23

app x = foldr (:) x



productq :: Num a => [a] -> a
productq = foldr (*) 1

data Tree a = Leaf a | Node (Tree a) a (Tree a)
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4) ) 5 (Node (Leaf 6) 7 (Leaf 9) ) 
contains :: Eq a => a -> Tree a -> Bool
contains x (Leaf y) = x == y
contains x (Node l y r) = x==y || contains x l || contains x r

data Dayq = Mon | Tue | Wed | Thu | Fri | Sat | Sun
 deriving (Eq, Ord, Show, Read)