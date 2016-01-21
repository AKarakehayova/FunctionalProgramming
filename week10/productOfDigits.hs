productOfDigits :: Int -> Int
productOfDigits 0 = 1
productOfDigits x = mod x 10 * productOfDigits (div x 10)
