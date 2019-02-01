{-# LANGUAGE DeriveFunctor #-}

newtype Box a = MkBox a deriving (Show, Functor)


--tozsame lub (mniej) z tym co wyzej
--instance Functor Box where
  -- fmap f (MkBox a) = MkBox (f a)
   

data MyList a = EmptyList
              | Cons a (MyList a) deriving (Show, Functor)

--instance Functor MyList where
 -- fmap _ EmptyList    = EmptyList
 -- fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

instance Functor BinTree where
 fmap _ EmptyBT = EmptyBT
 fmap f (NodeBT a lt rt) = NodeBT (f a) (fmap f lt) (fmap f rt)
 
 
  -- fmap should change the first element
  -- to jest dziwne...
newtype Pair b a = Pair { getPair :: (a,b) } deriving (Show)

instance Functor (Pair a) where
 fmap f (Pair (a,b)) = Pair (f a, b) 