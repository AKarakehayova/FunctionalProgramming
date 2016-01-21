occurrences :: [Int] -> [Int] -> [Int]
occurrences xs ys = [numberOfOccurrences x ys | x <- xs]

numberOfOccurrences :: Int -> [Int] -> Int
numberOfOccurrences element array = length  [x|x <- array , x == element]
