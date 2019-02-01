printHello = putStrLn "jeb"

printHello2 = putStrLn "jeb'"


vec3DLen :: (Double, Double,Double) -> Double
vec3DLen (x,y,z) = sqrt (x*x+y*y+z*z)

add3N :: Num x => x->x->x->x
add3N x y z = x + y + z           --nie mozna tabu uzywac i komentuje sie 
--tak i spacje sa potrzebne

swap :: (char,int) -> (int, char)
swap (x,y) = (y,x)

equalT :: (Num a, Eq a) => a->a->a->Bool
equalT x y z = if x==y && y==z 
then True
else False

sgn :: Int -> Int
sgn n = if n < 0
       then -1
       else if n == 0
            then 0
            else 1

absInt :: Int -> Int
absInt n = if n<0
        then -n
        else n

min2int :: Int->Int->Int
min2int x y = if x < y
              then x
              else y

min3int :: Int-> Int ->Int ->Int
min3int x y z = min2int (min2int x y) z   --i trzeba dawac te 
--nawiasy bo sie gubi :c













