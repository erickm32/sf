import Estado

data AExp = Num Int
  |Var String
  |Som AExp AExp
  |Sub AExp AExp
  |Mul AExp AExp
  |Div AExp AExp
  deriving(Show)

data BExp = TRUE
  | FALSE
  | Not BExp
  | And BExp BExp
  | Or  BExp BExp
  | Ig  AExp AExp
  deriving(Show)

data CExp = While BExp CExp
  | If BExp CExp CExp
  | Seq CExp CExp
  | Atrib AExp AExp
  | Skip
  deriving(Show)


abigStep :: (AExp,Estado) -> (Int,Estado)
abigStep (Var x,s) = (procuraVar s x,s)
abigStep (Num n,s) = (n,s)
abigStep (Som e1 e2,s)  = let(n1,s1) = abigStep (e1, s)
                             (n2,s2) = abigStep (e2, s)
                             in (n1+n2,s)
abigStep (Sub e1 e2,s)  = let(n1,s1) = abigStep (e1, s)
                             (n2,s2) = abigStep (e2, s)
                             in (n1-n2,s)
abigStep (Mul e1 e2,s)  = let(n1,s1) = abigStep (e1, s)
                             (n2,s2) = abigStep (e2, s)
                             in (n1*n2,s)
abigStep (Div e1 e2,s)  = let(n1,s1) = abigStep (e1, s)
                             (n2,s2) = abigStep (e2, s)
                             in (n1 `div` n2,s)
                             -- `div` pra dividir entre inteiros ._.

bbigStep :: (BExp,Estado) -> (Bool,Estado)
bbigStep (TRUE,s)  = (True,s)
bbigStep (FALSE,s) = (False,s)
bbigStep (Not b,s) = case bbigStep (b,s) of
                (True,_) -> (False, s)
                (False,_) -> (True, s)

bbigStep (Ig e1 e2,s ) = let(b1,s1) = abigStep (e1, s)
                            (b2,s2) = abigStep (e2, s)
                            in (b1 == b2, s)
bbigStep (And b1 b2,s ) = let(n1,s1) = bbigStep (b1, s)
                             (n2,s2) = bbigStep (b2, s)
                             in (n1 && n2, s)
bbigStep (Or b1 b2,s ) = let(n1,s1) = bbigStep (b1, s)
                            (n2,s2) = bbigStep (b2, s)
                            in (n1 || n2, s)

cbigStep :: (CExp,Estado) -> (CExp,Estado)
cbigStep (Skip,s)          = (Skip,s)
cbigStep (If b c1 c2,s) = case bbigStep(b,s) of
                (True, _) -> (c1, s)
                (False, _) -> (c2, s)
--cbigStep (Seq c1 c2,s)  =
--cbigStep (Atrib (Var x) e,s) =
--cbigStep (While b c, s) =


meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]


exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

teste1 :: BExp
teste1 = (Ig (Som (Num 3) (Num 3))  (Mul (Num 2) (Num 3)))
teste2 :: BExp
teste2 = (Ig (Som (Var "x") (Num 3))  (Mul (Num 2) (Num 3)))


--testec1 :: CExp
--testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y")))
--        (Atrib (Var "y") (Var "z")))

--fatorial :: CExp
--fatorial = (Seq (Atrib (Var "y") (Num 1))
--                (While (Not (Ig (Var "x") (Num 1)))
--                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
--                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
