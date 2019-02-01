not''' :: Bool -> Bool
not''' b = case b of
         True -> False
         False -> True

nand' :: (Bool,Bool) -> Bool
nand' (x,y) = case (x,y) of
              (False,False) -> True
              otherwise -> False

roots :: (Double,Double,Double) -> (Double,Double)
roots (a,b,c) = ( (-b - d)/e, (-b + d)/e)
      where d = b*b - 4*a*c 
            e = 2*a
roots' :: Double -> Double-> Double -> (Double,Double)
roots' x y z = ((-y - d)/e,(-y + d)/e)
      where d = y*y - 4*x*z
            e = 2*x
			
