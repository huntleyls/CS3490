import Control.Applicative hiding (Const)
-- Problem 1
data Safe a = Value a | Error String

instance Monad Safe where
    return = Value

    (Value x) >>= f = f x
    (Error s) >>= _ = Error s

-- Problem 2
data Ftree a = Leaf a | Node [Ftree a]

instance Functor Ftree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node ts) = Node (map (fmap f) ts)

instance Applicative Ftree where
    pure = Leaf
    Leaf f <*> t = fmap f t
    Node fs <*> t = Node [f <*> t | f <- fs]

instance Monad Ftree where
    return = Leaf
    Leaf a >>= f = f a
    Node ts >>= f = Node [t >>= f | t <- ts]

-- Problem 3
data Prop a = PVar a | Const Bool | And (Prop a) (Prop a)
            | Or (Prop a) (Prop a) | Not (Prop a) | Iff (Prop a) (Prop a)

instance Functor Prop where
    fmap f (PVar a) = PVar (f a)
    fmap _ (Const b) = Const b
    fmap f (And p1 p2) = And (fmap f p1) (fmap f p2)
    fmap f (Or p1 p2) = Or (fmap f p1) (fmap f p2)
    fmap f (Not p) = Not (fmap f p)
    fmap f (Iff p1 p2) = Iff (fmap f p1) (fmap f p2)

instance Applicative Prop where
    pure = PVar
    PVar f <*> p = fmap f p
    Const b <*> _ = Const b
    And p1 p2 <*> p = And (p1 <*> p) (p2 <*> p)
    Or p1 p2 <*> p = Or (p1 <*> p) (p2 <*> p)
    Not p <*> q = Not (p <*> q)
    Iff p1 p2 <*> q = Iff (p1 <*> q) (p2 <*> q)

instance Monad Prop where
    return = PVar
    PVar a >>= f = f a
    Const b >>= _ = Const b
    And p1 p2 >>= f = And (p1 >>= f) (p2 >>= f)
    Or p1 p2 >>= f = Or (p1 >>= f) (p2 >>= f)
    Not p >>= f = Not (p >>= f)
    Iff p1 p2 >>= f = Iff (p1 >>= f) (p2 >>= f)


-- Problem 4
data Lam a = Var a | App (Lam a) (Lam a) | Abs (Lam (Maybe a))

bindLam :: Lam a -> (a -> Lam b) -> Lam b
bindLam (Var x) f = f x
bindLam (App l1 l2) f = App (bindLam l1 f) (bindLam l2 f)
bindLam (Abs l) f = Abs (bindLam l (lift f))

instance Functor Lam where
    fmap f (Var x) = Var (f x)
    fmap f (App x y) = App (fmap f x) (fmap f y)
    fmap f (Abs x) = Abs (fmap (fmap f) x)

instance Applicative Lam where
    pure = Var
    (Var f) <*> x = fmap f x
    (App l1 l2) <*> x = App (l1 <*> x) (l2 <*> x)

lift :: (a -> Lam b) -> Maybe a -> Lam (Maybe b)
lift _ Nothing = Var Nothing
lift f (Just x) = fmap Just (f x)

instance Monad Lam where
    return = Var
    Var x >>= f = f x
    App x y >>= f = App (x >>= f) (y >>= f)
    Abs x >>= f = Abs (x >>= lift f)


-- Problem 5
data Poly a = Mono Double [a] | Sum (Poly a) (Poly a)

multiplyPoly :: Poly a -> Poly a -> Poly a
multiplyPoly (Mono c1 vars1) (Mono c2 vars2) = Mono (c1*c2) (vars1 ++ vars2)
multiplyPoly p (Sum q1 q2) = Sum (multiplyPoly p q1) (multiplyPoly p q2)
multiplyPoly (Sum p1 p2) q = Sum (multiplyPoly p1 q) (multiplyPoly p2 q)

instance Functor Poly where
    fmap f (Mono c vars) = Mono c (map f vars)
    fmap f (Sum x y) = Sum (fmap f x) (fmap f y)

instance Applicative Poly where
    pure x = Mono 1.0 [x]
    (Mono c1 fs) <*> (Mono c2 xs) = Mono (c1 * c2) [f x | f <- fs, x <- xs]
    (Sum f1 f2) <*> x = Sum (f1 <*> x) (f2 <*> x)
    x <*> (Sum y1 y2) = Sum (x <*> y1) (x <*> y2)

bindPoly :: (a -> Poly b) -> Poly a -> Poly b
bindPoly _ (Mono c []) = Mono c []
bindPoly f (Mono c vars) = foldl1 Sum $ map (\v -> multiplyPoly (Mono c []) (f v)) vars
bindPoly f (Sum x y) = Sum (bindPoly f x) (bindPoly f y)

instance Monad Poly where
    return x = Mono 1.0 [x]
    x >>= f = bindPoly f x



tests = safeTest ++ fTreetests ++ propTests ++ lamTests ++ polyTests
main = putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
getErrors = map fst . filter (not . snd) . zip [1..] $ tests

safeTest =
  [ ((unitSafe 0) == Value 0)
  , ((unitSafe 'a') == Value 'a')
  , (((<$>) (+3) (Value 5)) == Value 8)
  , (((<$>) (+3) (Error "error")) == Error "error")
  , (((<*>) (Value (+3)) (Value 5)) == Value 8)
  , (((<*>) (Value (+3)) (Error "error")) == Error "error")
  , (((>>=) (Value 5) (\x -> Value (x + 3))) == Value 8)
  , (((>>=) (Error "error") (\x -> Value (x + 3))) == Error "error")
  ]

fTreetests = 
  [
  ((unitFTree 0) == Leaf 0)
  , ((unitFTree 'a') == Leaf 'a')
  , (((<$>) (+3) (Node [Node [Leaf 1, Node [Leaf 2], Leaf 3], Leaf 4])) == Node [Node [Leaf 4,Node [Leaf 5],Leaf 6],Leaf 7])
  , (((<$>) (head) (Node [Node [Leaf "apple", Node [Leaf "banana"], Leaf "cherry"], Leaf "date"])) == Node [Node [Leaf 'a',Node [Leaf 'b'],Leaf 'c'],Leaf 'd'])
  , (((<*>) (Leaf (+3)) (Node [Node [Leaf 1, Node [Leaf 2], Leaf 3], Leaf 4])) == Node [Node [Leaf 4,Node [Leaf 5],Leaf 6],Leaf 7])
  , (((<*>) (Node [Leaf (*2),Leaf (+3)]) (Node [Node [Leaf 1, Node [Leaf 2], Leaf 3], Leaf 4])) == Node [Node [Node [Leaf 2,Node [Leaf 4],Leaf 6],Leaf 8],Node [Node [Leaf 4,Node [Leaf 5],Leaf 6],Leaf 7]])
  , (((<*>) (Node [Leaf (head),Leaf (last)]) (Node [Node [Leaf "apple", Node [Leaf "banana"], Leaf "cherry"], Leaf "date"])) == Node [Node [Node [Leaf 'a',Node [Leaf 'b'],Leaf 'c'],Leaf 'd'],Node [Node [Leaf 'e',Node [Leaf 'a'],Leaf 'y'],Leaf 'e']])
  , (((>>=) (Leaf 0) (\x -> Node [Leaf x, Leaf x])) == Node [Leaf 0,Leaf 0])
  , (((>>=) (Leaf 0) (\x -> Node [Node [Leaf x, Leaf x]])) == Node [Node [Leaf 0,Leaf 0]])
  , (((>>=) (Node [Node [Leaf 0, Leaf 1], Leaf 2]) (\x -> Node [Leaf x, Leaf x])) == Node [Node [Node [Leaf 0,Leaf 0],Node [Leaf 1,Leaf 1]],Node [Leaf 2,Leaf 2]])
  ]

propTests = 
  [
  ((unitProp 0) == PVar 0)
  , ((unitProp 'a') == PVar 'a')
  , (((<$>) (+3) (Or (And ((Const True)) (PVar 1)) (Not (PVar 3)))) == Or (And (Const True) (PVar 4)) (Not (PVar 6)))
  , (((<$>) (head) (Or (And ((Const True)) (PVar "x1")) (Not (Iff (PVar "x2") ((Const False)))))) == Or (And (Const True) (PVar 'x')) (Not (Iff (PVar 'x') (Const False))))
  , (((<*>) (PVar (+3)) (Or (And ((Const True)) (PVar 1)) (Not (PVar 3)))) == Or (And (Const True) (PVar 4)) (Not (PVar 6)))
  , (((<*>) (And (PVar (+3)) (PVar (*2))) (Or (And ((Const True)) (PVar 1)) (Not (PVar 3)))) == And (Or (And (Const True) (PVar 4)) (Not (PVar 6))) (Or (And (Const True) (PVar 2)) (Not (PVar 6))))
  , (((<*>) (Or (PVar head) (PVar last)) (Or (And ((Const True)) (PVar "x1")) (Not (Iff (PVar "x2") ((Const False)))))) == Or (Or (And (Const True) (PVar 'x')) (Not (Iff (PVar 'x') (Const False)))) (Or (And (Const True) (PVar '1')) (Not (Iff (PVar '2') (Const False)))))
  , (((>>=) (PVar 'x') (\a -> PVar (a:"y"))) == PVar "xy")
  , (((>>=) (And (PVar 'z') (PVar 'x')) (\a -> PVar (a:"y"))) == And (PVar "zy") (PVar "xy"))
  , (((>>=) (And (PVar 'z') (PVar 'x')) (\a -> Iff (PVar (a:"y")) (PVar (a:"1")))) == And (Iff (PVar "zy") (PVar "z1")) (Iff (PVar "xy") (PVar "x1")))
  ]

lamTests = 
  [
  ((unitLam 0) == Var 0)
  , ((unitLam 'a') == Var 'a')
  , (((<$>) (head) (Abs (App (Var (Just "hello")) (Var Nothing)))) == Abs (App (Var (Just 'h')) (Var Nothing)))
  , (((<*>) (Var (+3)) (Var 1)) == Var 4)
  , (((<*>) (Var (+3)) (App (Var 0) (Var 1))) == App (Var 3) (Var 4))
  , (((<*>) (App (Var (+3)) (Var (*2))) (App (Var 0) (Var 1))) == App (App (Var 3) (Var 4)) (App (Var 0) (Var 2)))
  , (((>>=) (Var 0) (\x -> Var (x + 3))) == Var 3)
  , (((>>=) (Var 0) (\x -> Abs (Var (Just (x + 3))))) == Abs (Var (Just 3)))
  , (((>>=) (App (Var 1) (Var 2)) (\x -> App (Var x) (Var (-x)))) == App (App (Var 1) (Var (-1))) (App (Var 2) (Var (-2))))
  ]

polyTests = 
  [
  ((unitPoly 0) == Mono 1.0 [0])
  , ((unitPoly 'a') == Mono 1.0 "a")
  , ((polyA >>= q1) `eqPoly` Mono 1.0 [])
  , ((polyA >>= q3) `eqPoly` Mono 1.0 [])
  , ((polyC >>= q2) `eqPoly` (Sum (Sum (Sum (Mono 3.0 ["X","X"]) (Mono 6.0 ["X"])) (Sum (Mono 6.0 ["X"]) (Mono 12.0 []))) (Sum (Mono (-1.0) ["X","X"]) (Mono (-2.0) ["X"]))))
  , ((polyD >>= q3) `eqPoly` (Sum (Sum (Mono 23.0 ["X","X","X"]) (Mono 23.0 ["Y","X","X"])) (Sum (Mono 1.0 ["X","X","X"]) (Mono 1.0 ["Y","X","X"]))))
  , (( (polyE >>= q2) >>= q3) `eqPoly` (Sum (Mono 2.0 ["X","X","X","X"]) (Mono 4.0 ["X","X"])))
  , (( (polyF >>= q3) >>= q1) `eqPoly` (Sum (Sum (Mono 16.0 ["Y","Y","Y","Y"]) (Sum (Mono 8.0 ["Y","Y","Y"]) (Mono 24.0 ["Y","Y","Z","Z","Y"]))) (Sum (Sum (Mono 8.0 ["Y","Y","Y"]) (Mono 24.0 ["Y","Y","Y","Z","Z"])) (Sum (Sum (Mono 4.0 ["Y","Y"]) (Mono 12.0 ["Y","Y","Z","Z"])) (Sum (Mono 12.0 ["Y","Y","Z","Z"]) (Mono 36.0 ["Y","Y","Z","Z","Z","Z"]))))))
  , ((polyB >>= q2) `eqPoly` (Mono 8.0 ["X","X"]))
  , ((polyD >>= q1) `eqPoly` (Sum (Sum (Mono 46.0 ["Y"]) (Mono 138.0 ["Z","Z","Y"])) (Sum (Mono 2.0 ["Y"]) (Mono 6.0 ["Z","Z","Y"]))))
  ]

eval :: (a -> Double) -> Poly a -> Double
eval f (Mono x ys) = foldr (*) x (map f ys)
eval f (Sum p1 p2) = eval f p1 + eval f p2

f1 :: String -> Double
f1 "X" = 4
f1 "Y" = 14
f1 "Z" = 7

f2 :: String -> Double
f2 "X" = -1
f2 "Y" = 7
f2 "Z" = -5

f3 :: String -> Double
f3 "X" = 12
f3 "Y" = 1
f3 "Z" = 5

f4 :: String -> Double
f4 "X" = 2
f4 "Y" = 3
f4 "Z" = 5

f5 :: String -> Double
f5 "X" = 0
f5 "Y" = 5
f5 "Z" = 2

evalAtList :: [a -> Double] -> Poly a -> [Double]
evalAtList lst p = flip eval p <$> lst

eqPoly :: Poly String -> Poly String -> Bool
eqPoly p1 p2 = evalAtList [f1,f2,f3,f4,f5] p1 == evalAtList [f1,f2,f3,f4,f5] p2

q1 "X" = Mono 2.0 ["Y"]
q1 "Y" = Sum (Mono 1.0 []) (Mono 3 ["Z","Z"])
q1 "Z" = Mono 1.0 ["X"]
q2 "X" = Mono 2.0 ["X"]
q2 "Y" = Sum (Mono 1.0 ["X"]) (Mono 2.0 [])
q2 "Z" = Mono 1.0 ["Y"]
q3 "X" = Mono 1.0 ["X","X"]
q3 "Y" = Sum (pure "X") (pure "Y")
q3 "Z" = Sum (Mono 1.0 ["X"]) (Mono 1.0 ["Y"])
polyA = Mono 1.0 []  
polyB = Mono 2.0 ["X", "X"] 
polyC = Sum (Mono 3.0 ["Y", "Y"]) (Mono (-0.5) ["X","Y"]) 
polyD = Sum (Mono 23.0 ["X", "Y"]) (Mono 1.0 ["X", "Y"])
polyE = Mono 1.0 ["X", "Y"]
polyF = Mono 1.0 ["X", "Y", "Z"]
