-- Quick answer to question one (just based off the algorithm, not the steps)
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

-- Question two
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,b)]
hanoi 2 a b c = [(a,c),(a,b),(c,b)]
hanoi n a b c = concat [(hanoi (n-1) a c b), [(a, b)], (hanoi (n-1) c b a)]
