polar_tc :: Floating a => (a,a) -> (a,a)
polar_tc (r,phi) = (r * cos phi,r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

personInfoToString :: (String,String,String) -> String
personInfoToString (nm,snm,addr) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr
 
type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfo' -> String
personInfoToString' (name,surname,address) = "name: " ++ name ++ surname ++ address

newtype PersonInfo'' = MkPersonInfo'' (String,String,String)

personInfoToString'' :: PersonInfo'' -> String
personInfoToString'' (MkPersonInfo'' (n,s,a)) = n++s++a 
--wywolujemy od Mk.... albo od czegos za co podstawilismy person w sposob
--let x = MkPersonInfo''.....








