suffix :: (Eq a, Num a) => [a] -> [a] -> Bool 
suffix xs ys = null . filter (/= 0) $ zipWith (-) xs yss
 where
 	l = (length ys - length xs)
 	yss = drop l ys
