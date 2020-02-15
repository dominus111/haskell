import Control.Exception
import System.IO
import System.Environment (getArgs)
import Tokens

main = do
        args <- getArgs
        file <- openFile (head args) ReadMode
        text <- hGetContents file
        let liness = (alexScanAll (lines text))
        print liness

alexScanAll :: [String] -> [[Token]]
alexScanAll [] = []
alexScanAll (x:xs) = alexScanTokens x : alexScanAll xs 
