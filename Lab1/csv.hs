import Data.List
import Data.Char
import System.Environment
import System.IO

--main:: IO ()
main = do
  let fileName = "input.csv"
  input <- readFile fileName
  let res = lines input
  let newContent = parseT res
  writeFile "output.csv" ""
  some newContent


writeF (x:[]) = appendFile "output.csv" ((show x) ++ "\n")
writeF (x:xs) = do 
    appendFile "output.csv" ((show x) ++ ",")
    writeF xs

isEmpty2 [] = []  
isEmpty2 (x:xs) | length x == 0 = isEmpty2 xs
                | otherwise = x:isEmpty2 xs

some [] = return ()
some (x:xs) = do 
 writeF x
 some xs

parseT [] = []
parseT xs = map takeS xs : parseT (isEmpty2 (map behead xs))

behead (x:[]) = []
behead (x:y:cs) = cs

takeS (x:_) | isDigit x = (digitToInt x)
            | otherwise = error "Non digit character"