class Mappable t where
  fMap :: (a -> b) -> t a -> t b

data Vec3D a = Vec3D {x::a, y::a, z::a} deriving Show

instance Mappable Vec3D where
  fMap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)
  
 

newtype Pair a = Pair (a,a) deriving Show

instance Mappable Pair where
  fMap f (Pair (x,y)) = Pair (f x, f y)
  
data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)
                 deriving Show


instance Mappable BinTree where
 fMap f (EmptyBT) = EmptyBT
 fMap f (NodeBT a lt rt) = NodeBT (f a) (fMap f lt) (fMap f rt)
 

instance Mappable Maybe where
 fMap f (Nothing) = Nothing
 fMap f (Just a) = Just (f a)
 
data Either' a = Either' {left::a,right::a} deriving Show
instance Mappable Either' where
 fMap f (Either' a b) = Either' (f a) (f b)
 
instance Mappable ((->) a) where
  fMap f g = f.g
  
  
class VectorLike t where
 (|==|) :: Eq a => t a -> t a -> Bool
 (|+|), (|-|) :: (Num a) => t a -> t a -> t a
 (|*|) :: (Num a) => t a -> t a -> a
 (||?), (|-?) :: (Num a, Eq a) => t a -> t a -> Bool -- równoległość i prostopadłość
 vectLength :: Floating a => t a -> a
 unitVectOf :: Floating a => t a -> t a
 
data Vec2D a = Vec2D {g::a, h::a}

instance VectorLike Vec2D where
 (|==|) (Vec2D {g=a1,h=b1}) (Vec2D {g=a2, h=b2}) = (a1 == a2) && (b1 == b2)
 (|+|) (Vec2D {g=a1,h=b1}) (Vec2D {g=a2, h=b2}) = Vec2D {g=a1+a2, h= b1 + b2}
 (|-|) (Vec2D {g=a1,h=b1}) (Vec2D {g=a2, h=b2}) = Vec2D {g=a1-a2, h= b1 - b2}
 (|*|) (Vec2D {g=a1,h=b1}) (Vec2D {g=a2, h=b2}) = a1*a2+ b1*b2
 (||?) (Vec2D {g=a1,h=b1}) (Vec2D {g=a2, h=b2}) = a1*b2 == a2 * b1
 (|-?) (Vec2D {g=a1,h=b1}) (Vec2D {g=a2, h=b2}) = a1*a2 + b1 * b2 == 0
 vectLength (Vec2D {g=a1,h=b1}) = sqrt(a1*a1+b1*b1)
 unitVectOf (Vec2D {g=a1,h=b1}) = Vec2D {g=a1/sqrt(a1*a1+b1*b1), h= b1/sqrt(a1*a1+b1*b1)}
 
 
 