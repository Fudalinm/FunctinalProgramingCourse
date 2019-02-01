
data Expr a = Lit a |
              Op Ops (Expr a) (Expr a) |
              If (BExpr a) (Expr a) (Expr a)

data Ops = Add | Sub | Mul

data BExpr a = BoolLit Bool |
               And (BExpr a) (BExpr a) |
               Or (BExpr a) (BExpr a) |
               Not (BExpr a) |
               Equal (Expr a) (Expr a) |
               Greater (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Op Add e1 e2) = eval e1 + eval e2
eval (Op Sub e1 e2) = eval e1 - eval e2
eval (Op Mul e1 e2) = eval e1 * eval e2
eval (If b e1 e2) = if (bEval b) == True
                       then eval e1
                       else eval e2
					   

bEval :: BExpr a -> Bool
bEval (BoolLit b) = b
bEval (Not a) = not (bEval a)
bEval (And e1 e2) = (bEval e1) && (bEval e2)
bEval (Or e1 e2) = bEval e1 || bEval e2
bEval (Equal e1 e2) = eval e1 == eval e2
bEval (Greater e1 e2) =  eval e1 > eval e2


