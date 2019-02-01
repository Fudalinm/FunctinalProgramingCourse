absInt :: Int -> Int
absInt n | n > 0 = n
         | n <= 0 = -n


not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer "LOVE" = True
isItTheAnswer _      = False

or' :: Bool -> Bool -> Bool
or' x y = if x==False && y==False
        then  False
        else  True

or'' :: (Bool,Bool) -> Bool
or'' (False,False) = False
or'' _           = True

and' :: (Bool,Bool) -> Bool
and' (True,True) = True
and'  _          = False

xor' :: (Bool,Bool) -> Bool
xor'    (True,False) = True
xor'    (False,True) = True
xor'     _          = False













