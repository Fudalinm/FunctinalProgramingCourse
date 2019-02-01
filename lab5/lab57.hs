--roznica miedzy *> a <*
--dlaczego jedno zaawsze zwraca lewe a drugie zawsze prawe za wyjatkiem left!



newtype Box a = MkBox a deriving Show

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w
  
instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)
  
newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Functor MyTriple where
 fmap f (MyTriple (a,b,c)) = MyTriple (f a, f b, f c)
 
instance Applicative MyTriple where
pure a = MyTriple (a,a,a)
(MyTriple (a,b,c) ) <*> (MyTriple (x,y,z)) = (x a,y b,z c)
  
  
data Tree2 a = EmptyT2 | Leaf a | Node (Tree2 a) a (Tree2 a) deriving Show

instance Functor Tree2 where
	fmap _ EmptyT2 = EmptyT2
	fmap f (Leaf a) = Leaf (f a)
	fmap f (Node lt a rt) = Node (fmap f lt) (f a) (fmap f rt)
	


instance Applicative Tree2 where
   pure x = Node (pure x) x (pure x)
   EmptyT2 <*> _ = EmptyT2
   _ <*> EmptyT2 = EmptyT2
   Leaf f <*> Leaf x = Leaf (f x)
   Leaf f <*> (Node _ r _) = Leaf (f r)
   (Node _ f _) <*> Leaf r = Leaf (f r)
   (Node lt f rt) <*> (Node lt2 r rt2) = Node (lt <*> lt2) (f r) (rt <*> rt2)
























































