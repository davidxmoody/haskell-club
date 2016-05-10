doubleSecond :: [Int] -> [Int]
doubleSecond [] = []
doubleSecond (x:(y:ys)) = (x * 2) : (y : doubleSecond ys)

sumDigits :: Int -> Int
sumDigits n
  | n < 10    = n
  | otherwise = (n `mod` 10) + sumDigits (n `div` 10)

getTotal :: [Int] -> Int
getTotal xs = sum(map (sumDigits) (doubleSecond xs))

isValid :: [Int] -> Bool
isValid xs = (getTotal xs) `mod` 10 == 0
