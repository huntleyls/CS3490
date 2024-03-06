-- Problem 1.1
mapAppend :: (a -> [b]) -> [a] -> [b] 
mapAppend _ [] = []
mapAppend f (x:xs) = f x ++ mapAppend f xs

-- Problem 1.2
addLetter :: Char -> [String] -> [String]
addLetter _ [] = []
addLetter c (s:strs) = (c : s)  : addLetter c strs 

-- Problem 1.3
addLetters :: [Char] -> [String] -> [String]
addLetters [] _ = []
addLetters (c:cs) strs = addLetter c strs ++ addLetters cs strs

-- Problem 1.4
makeWords :: [Char] -> Integer -> [String]
makeWords _ 0 = [""]
makeWords chars n = addLetters chars (makeWords chars (n-1))

type Vars = String

data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
  deriving Show
  
prop1 = Var "X" `And` Var "Y"
prop2 = Var "X" `Or` Var "Y"
prop3 = Not (Var "X") `Or` Var "Y"
prop4 = Not (Var "X") `Or` Not (Var "Y")