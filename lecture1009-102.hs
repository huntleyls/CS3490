data Parser a = ParserC (String -> [(String,a)])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (ParserC p) = ParserC g  -- g :: String -> [(String,b)]
    where g s = fmap (\(x,y) -> (x,f y)) (p s)

{-
data Parser a = ParserC (String -> [a])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (ParserC p) = ParserC (fmap f . p)
  -- fmap f (ParserC p) = ParserC (\s -> fmap f (p s))
  -- fmap f (ParserC p) = ParserC q where -- q :: String -> [b]
  --                        q s = fmap f (p s)
-}

data Comb a = Var a | I | K | S | App (Comb a) (Comb a)

instance Functor Comb where
  -- fmap :: (a -> b) -> Comb a -> Comb b
  fmap f (Var x) = Var (f x)
  fmap f I = I
  fmap f K = K
  fmap f S = S
  fmap f (App c1 c2) = App (fmap f c1) (fmap f c2)

bindComb :: (a -> Comb b) -> Comb a -> Comb b
bindComb f (Var x) = f x
bindComb f I = I
bindComb f K = K
bindComb f S = S
bindComb f (App c1 c2) = App (bindComb f c1) (bindComb f c2)

-- unitComb :: a -> Comb a
-- unitComb = Var

instance Applicative Comb where
  -- pure :: a -> Comb a
  pure = Var
  cf <*> cx = bindComb (<$> cx) cf
  -- cf <*> cx = bindComb g cf where g f = fmap f cx

instance Monad Comb where
  return  = pure
  c >>= f = bindComb f c

ex1 :: Comb Integer
ex1 = App (App (App S (Var 0)) K) (Var 3)

ex2 :: Comb Bool
ex2 = App (App K (Var True)) (App I (Var False))

myFun :: Integer -> Comb String
myFun 0 = App I (Var "x")
myFun 3 = App (Var "y") (Var "z")

instance Show a => Show (Comb a) where
  -- show :: Eq a => Comb a -> String
  show (Var x) = show x
  show I = "I"
  show K = "K"
  show S = "S"
  show (App c1 c4@(App c2 c3)) = show c1 ++ "(" ++ show c4 ++ ")"
  show (App c1 c2) = show c1 ++ show c2

instance Eq a => Eq (Comb a) where
  -- (==) :: Eq a => Comb a -> Comb a -> Bool
  Var x == Var y = x == y
  I == I  = True
  K == K  = True
  S == S  = True
  (App c1 c2) == (App d1 d2) = (c1 == d1) && (c2 == d2)
  _ == _  = False

-- ex :: Comb -- SIK(KI)
-- ex = App (App (App S I) K) (App K I)

-- Lists
-- Trees
-- ??

unitList :: a -> [a]
unitList x = [x]

mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = f x : mapList f xs

bindList :: (a -> [b]) -> [a] -> [b]
bindList f [] = []
bindList f (x:xs) = f x ++ bindList f xs

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

exTree :: Tree Integer
exTree = Node 5 (Node 3 Leaf Leaf)
                (Node 7 (Node 0 Leaf Leaf)
                        (Node 1 Leaf Leaf))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1) (mapTree f t2)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap = mapTree

unitTree :: a -> Tree a
unitTree x = Node x Leaf Leaf
bindTree :: (a -> Tree b) -> Tree a -> Tree b
bindTree f t = error ""
