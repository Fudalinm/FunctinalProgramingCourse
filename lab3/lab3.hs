sumWith :: Num a => (a->a) -> [a] -> a
sumWith _ [] = 0
sumWith f (x:xs) = (f (x)) + (sumWith f (xs))

cube :: Num a => a -> a
cube x = x*x*x

squere :: Num a => a -> a
squere x = x*x 





