
zipL :: Num a => ([a],[a]) -> [[a]]
zipL ([],[]) = []
zipL ([],(y:ys)) = [y] : zipL ([],ys)
zipL ((x:xs),[]) = [x] : zipL (xs,[])
zipL ((x:xs),(y:ys)) = [x,y] : zipL (xs,ys)

unzipL :: Num a => [[a]] -> ([a], [a])
unzipL xs = (map (head) xs, map (last) xs) 