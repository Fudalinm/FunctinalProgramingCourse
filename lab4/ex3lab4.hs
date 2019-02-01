data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a) |
              Sub (Expr a) (Expr a) |
              Mul (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = (eval e1) * (eval e2)

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Sub e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Mul e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"


data BinTree a= EmptyT | Node a (BinTree a) (BinTree a) deriving Show 

depthOfBT :: BinTree a -> Int 
depthOfBT EmptyT = 0
depthOfBT (Node a lt rt) = if depthOfBT lt > depthOfBT rt
                           then 1 + depthOfBT lt
                           else 1 + depthOfBT rt
--inorder od najmniejszego do najwiekszego
--pre order najpierw ten w ktorym jestesmy potem jego lewe poddrzewo potem jego prawe poddrzewo
--post ordernajpier dzieci z lewego potem dzieci z prawego potem siebie

flattenBT :: BinTree a -> [a]
flattenBT EmptyT = []
flattenBT (Node a lt rt) = flattenBT lt ++ [a] ++ flattenBT rt

flattenBT' :: BinTree a -> [a]
flattenBT' EmptyT = []
flattenBT' (Node a lt rt) =  [a] ++ flattenBT lt ++ flattenBT rt 

flattenBT'' :: BinTree a -> [a]
flattenBT'' EmptyT = []
flattenBT'' (Node a lt rt) =   flattenBT lt ++ flattenBT rt ++ [a]

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ EmptyT = EmptyT
mapBT f (Node a lt rt) = Node (f a) (mapBT f lt) (mapBT f rt) 

insert :: Ord a => a -> BinTree a -> BinTree a
insert a EmptyT = Node a (EmptyT) (EmptyT)
insert a (Node x lt rt) = if  a > x
then insert a lt
else insert a rt

list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyT
list2BST (x:xs) = Node x (list2BST (filter (x >) xs)) (list2BST (filter (x <= ) xs))

occurs :: Eq a => a -> BinTree a -> Int -- liczba wystąpień elementu w drzewie binarnym
occurs _ EmptyT = 0
occurs x (Node a lt rt) = if a == x
                          then 1 + occurs a lt + occurs a rt
                          else occurs a lt + occurs a rt

elemOf :: Eq a => a -> BinTree a -> Bool -- sprawdzenie, czy element znajduje się w drzewie
elemOf _ EmptyT = False
elemOf a (Node x lt rt) = if x == a
                          then True
                          else elemOf a lt || elemOf a rt

reflect :: BinTree a -> BinTree a -- 'odbicie lustrzane' drzewa binarnego
reflect EmptyT = EmptyT
reflect (Node a lt rt) = (Node a rt lt)

minElemOf :: Ord a => BinTree a -> a
minElemOf EmptyT = error "nie ma najmniejszego z niczego "
minElemOf (Node a (EmptyT) rt) = a
minElemOf (Node a lt rt) = minElemOf lt

data GTree a = Leaf a |
               GNode [GTree a]
               deriving Show

sumGTree :: Num a => GTree a -> a
sumGTree (Leaf a) = a
sumGTree (GNode []) = 0
sumGTree (GNode (x:xs)) = sumGTree x + sumGTree (GNode xs)

elemOfGTree :: Eq a => a -> GTree a -> Bool
elemOfGTree _ (GNode []) = False
elemOfGTree a (Leaf x) = if x == a
              then True 
              else False
elemOfGTree a (GNode (x:xs)) = elemOfGTree a x || elemOfGTree a (GNode xs)

depthOfGTree :: GTree a -> Int
depthOfGTree (Leaf a) = 0
depthOfGTree (GNode []) = 0
depthOfGTree (GNode (x:xs)) = 1 + max (depthOfGTree x) (depthOfGTree (GNode xs))





