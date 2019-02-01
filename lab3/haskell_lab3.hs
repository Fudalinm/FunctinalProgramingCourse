import Data.List

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs



prodWith :: Num a => (a-> a) -> [a] -> a
prodWith _ [] = 1
prodWith f (x:xs) = f x * prodWith f xs

sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

df :: (Double -> Double) -> Double -> (Double -> Double)
df f h = \x -> (f (x + h) - f (x - h)) / (2 * h)


d2f :: (Double->Double) -> Double -> (Double->Double)
d2f f h = \x -> ( f(x+h) - 2*(f(x)) + f(x-h))/(h^2)

fac :: Int -> Int
fac n | n < 0 = error "blad"
      | n==0 = 1
      | otherwise = n*fac(n-1)

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = (\x -> sum [(x^k)/fromIntegral(fac k) | k <- [0..n]])

funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x ]

evalFuncListAt :: Double -> [Double -> Double] -> [Double]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)

funcList' :: [Double->Double]
funcList' = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x, \x -> sqrt (1 + x) ]


sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

sortDesc2 :: Ord a => [a] -> [a]
sortDesc2 xs = reverse (sort xs)

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt _ _ [] = True
are2FunsEqAt f g (x:xs) = if f x == g x 
                          then are2FunsEqAt f g xs
                          else False

composeFunList :: [a -> a] -> (a -> a)
composeFunList [] = id
composeFunList (f:fs) = \x -> (f . composeFunList fs) x

doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs



