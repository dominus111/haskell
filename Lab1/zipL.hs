zipL :: Num a => ([a],[a]) -> [[a]]
zipL (xs,ys) | length xs == length ys = zipP (xs,ys)
             | otherwise = error "Not even length"

zipP :: Num a => ([a],[a]) -> [[a]]
zipP ([],[]) = []
zipP ((x:xs),(y:ys)) = [x,y] : zipP (xs,ys)

unzipL :: Num a => [[a]] -> ([a], [a])
unzipL xs = (map (head) xs, map (last) xs) 