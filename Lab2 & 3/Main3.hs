import Control.Exception
import System.IO
import System.Environment (getArgs)
import Tokens
import Grammar

-- not working for lines!!
main = catch ( do
--		main catch main' errorFun
        args <- getArgs
        file <- openFile (head args) ReadMode
        text <- hGetContents file
        parseCalc' $alexScanAll ( lines text)
        ) errorFun


alexScanAll :: [String] -> [[Token]]
alexScanAll [] = []
alexScanAll (x:xs) = alexScanTokens x : alexScanAll xs 

parseCalc' [] = putStrLn $"EOF"
parseCalc' (x:xs) = do 
                    print (parseCalc x)  
                    parseCalc' xs
errorFun :: SomeException -> IO()
errorFun ex = putStrLn $"Exception !" ++ show ex
