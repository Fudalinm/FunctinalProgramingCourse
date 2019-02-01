newtype MyInt = MkMyInt Int

instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2
  
instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)
  
instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i
  
data BinTree a = EmptyT | Node a (BinTree a) (BinTree a)

instance Eq a => Eq (BinTree a) where
  EmptyT == EmptyT = True
  EmptyT == _ = False
  _ == EmptyT = False
  Node a lt rt == Node a2 lt2 rt2 = ( a == a2 && lt == lt2 && rt == rt2 )

data Fraction a = Fraction {num::a , denom::a}

instance Eq a => Eq (Fraction a) where
  Fraction a b == Fraction a2 b2 = a == a2 && b == b2

instance Show a => Show (Fraction a) where
  show (Fraction a b) = show a ++ "podzielone przez" ++ show b

instance Ord a => Ord (Fraction a) where
  Fraction a b <= Fraction a2 b2 = b2 < a2
  
