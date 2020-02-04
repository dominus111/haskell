multiZipL :: Eq a => [[a]] -> [[a]]
multiZipL [[]] = []
multiZipL xs = [x | (x:_) <- xs]  :  multiZipL [ xs | (_:xs) <- xs]