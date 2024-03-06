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

-- 1. Function to list all variables occurring in a given formula
fv :: Prop -> [Vars]
fv (Var v) = [v]
fv (Const _) = []
fv (And p1 p2) = fv p1 ++ fv p2
fv (Or p1 p2) = fv p1 ++ fv p2
fv (Not p) = fv p

-- 2. Function to look up a variable in a list of key-value pairs
lookUp :: Vars -> [(Vars, Bool)] -> Bool
lookUp key ((k, v) : rest)
  | key == k = v
  | otherwise = lookUp key rest
lookUp _ _ = False  -- Default value when variable is not found

-- 3. Evaluator function using truth table semantics
eval :: [(Vars, Bool)] -> Prop -> Bool
eval _ (Const b) = b
eval env (Var v) = lookUp v env
eval env (And p1 p2) = eval env p1 && eval env p2
eval env (Or p1 p2) = eval env p1 || eval env p2
eval env (Not p) = not (eval env p)

-- 4. Function to evaluate a formula in a list of environments
evalList :: Prop -> [[(Vars, Bool)]] -> Bool
evalList _ [] = False
evalList p (env:envs) = eval env p || evalList p envs

-- 5. Function to generate all possible environments for a list of variables
genEnvs :: [Vars] -> [[(Vars, Bool)]]
genEnvs [] = [[]]
genEnvs (var:vars) = concatMap (extend var) (genEnvs vars)
  where
    extend v env = [(v, True) : env, (v, False) : env]

-- 6. Function to check if a formula is satisfiable
sat :: Prop -> Bool
sat formula = evalList formula (genEnvs (fv formula))