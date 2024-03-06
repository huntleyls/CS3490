-- Bubble sort
-- part 1
bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:rest) 
    | x > y     = y : bubble (x : rest)
    | otherwise = x : bubble (y : rest)

-- part 2
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs 
    | xs == bubbled = xs 
    | otherwise     = bubbleSort bubbled
    where
        bubbled = bubble xs

-- Generating, searching and replacing strings
-- part 1
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys

isSubstring :: String -> String -> Bool
isSubstring _ [] = False
isSubstring xs ys 
    | isPrefix xs ys = True
    | otherwise      = isSubstring xs (tail ys)

-- part 2
genTails :: String -> [String]
genTails "" = []
genTails s = s : genTails (tail s)

genPrefix :: String -> [String]
genPrefix s = [take i s | i <- [1..length s]]

-- part 3
genSubstrings :: String -> [String]
genSubstrings xs = "" : concatMap genPrefix (genTails xs)

-- part 4
replacePrefix :: (String,String) -> String -> String
replacePrefix (old, new) str = new ++ drop (length old) str

-- part 5
replaceString :: (String, String) -> String -> String
replaceString (old, new) str
    | isPrefix old str = new ++ drop (length old) str
    | otherwise = head str : replaceString (old, new) (tail str)

-- a Simple Cypher
-- part 1
lookUp :: Char -> [(Char,Char)] -> Char
lookUp x [] = x
lookUp x ((k,v):rest) 
    | x == k    = v 
    | otherwise = lookUp x rest

-- part 2
encode :: [(Char,Char)] -> String -> String
encode table = map (\x -> lookUp x table)

-- part 3
makeTable :: String -> String -> [(Char,Char)]
makeTable xs ys = zip xs ys

caesar :: Int -> [(Char,Char)]
caesar n = makeTable abc (drop n (cycle abc)) where abc = ['A'..'Z']