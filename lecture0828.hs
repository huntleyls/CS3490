-- Lecture 08.28
--addList :: [Integer] -> Integer
--addList [] = 0
--addList (x:xs) = x + addList xs
-- addList = head xs + addList (tail xs)

allOdds :: [Integer] -> Bool
allOdds [] = True
allOdds (x:xs) = (odd x) && (allOdds xs)
-- allOdds (x:xs) = if x 'mod 2 = 0 then False else allOdds xs
-- allOdds (x:xs) | x 'mod' 2 == 0 False
-- allOdds (x:xs) = allOdds xs

multiplyPairs :: [(Integer,Integer)] -> [Integer]
multiplyPairs [] = []
multiplyPairs ((x1,x2):xs) = (x1 * x2) : (multiplyPairs xs)

findPalindrome1 :: [String] -> Bool
findPalindrome1 [] = False
findPalindrome1 (x:xs) = if reverse x == x then True
                                           else findPalindrome1 xs
findPalindromes :: [String] -> [String]
findPalindromes [] = []
findPalindromes (x:xs) | reverse x == x = Just x
                        | otherwise      = findPalindromes xs