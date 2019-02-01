
data Int2DVec = MkInt2DVec Int Int 

xCord :: Int2DVec -> Int
xCord (MkInt2DVec x _) = x 

yCord :: Int2DVec -> Int
yCord (MkInt2DVec _ y) = y


data Cart2DVec' a = MkCart2DVec' a a 
--przyjmuje tez sewencje charow mozna bylo tak zadeklarowa osobe

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y


data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

xCoord'' :: Cart2DVec'' a -> a
xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

yCoord'' :: Cart2DVec'' a -> a
yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- dlaczego taka a nie inna kolejnosc?
--moze dlatego ze y i x jest nasz funkcja 

--x:: jest tak naprawde deklaracja funkcji
--wiec oczywiscie powyzsze 2 funkcje sa niepotrzebne

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x 
--Działa Cons 1 $ Cons 2 $ EmptyL tylko przy ostatnim nie trzeba dolara

-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"


data Cart3DVec a = Cart3DVec a a a

xCoord3 :: Cart3DVec a -> a
xCoord3 (Cart3DVec a _ _) = a


data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle a) = pi*a*a 
area (Rectangle a b) = a*b

data Tree a = EmptyT |
              Node a (Tree a) (Tree a)
              deriving Show

root :: Tree a -> a 
root EmptyT = error "root: nie ma zadnego drzewa"
root (Node a lt rt) = a
--wywołujemy to normalnie np root EmptyT lub root (Node 4 EmptyT EmptyT)






