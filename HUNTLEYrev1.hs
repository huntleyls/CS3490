-- Problem 1
radius :: Double -> Double -> Double
radius x y = sqrt (x^2 + y^2)

-- Problem 2
radius' :: (Double, Double) -> Double
radius' (x, y) = sqrt (x^2 + y^2)

-- Problem 3
sumEvens :: Integer -> Integer
sumEvens n
  | n <= 0    = 0
  | even n    = n + sumEvens (n - 2)
  | otherwise = sumEvens (n - 1)

-- Problem 4
sumEvens' :: Integer -> Integer
sumEvens' n = sum [x | x <- [1..n], even x]

-- Problem 5
collatz :: Integer -> Integer
collatz 0 = 1
collatz 1 = 1
collatz n
  | even n    = collatz (n `div` 2)
  | otherwise = collatz (3 * n + 1)

-- Problem 6
collatzCheck :: [Integer]
collatzCheck = [collatz n | n <- [1..100]]

-- Problem 7
multiplesOfFive :: [Integer]
multiplesOfFive = [n | n <- [1..100], n `mod` 5 == 0]

-- Problem 8
init' :: [a] -> [a]
init' [] = error "The list is empty"
init' [_] = []
init' (x:xs) = x : init' xs

-- Problem 9
findEmpty :: [String] -> Bool
findEmpty = any null

-- Problem 10
getLengths :: [String] -> [Int]
getLengths strs = [length str | str <- strs]
