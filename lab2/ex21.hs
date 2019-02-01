myfun x = 2*x

add2T :: Num a => (a,a) -> a 
add2T (x,y) = x + y

add2C :: Num a => (a -> (a -> (a))) --lewostronna lacznosc
add2C x y = x + y

fiveToPower_ :: Integer -> Integer
fiveToPower_ x = (5 ^) x



