split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar ch (c:cs)
    | c == ch   = removeChar ch cs
    | otherwise = c:(removeChar ch cs)

main = do
    handle <- openFile "input.csv" ReadMode
    contents <- hGetContents handle
    let names = sort (map (removeChar '"') (split ',' contents))
    print names
    hClose handle