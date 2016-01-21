removeAt :: Int -> [a] -> [a]
removeAt index array
    |index < 0 || index >= length array = error "Index out of bounds"
    |otherwise = take index array ++ drop (index + 1) array
