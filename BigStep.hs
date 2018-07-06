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
  | Leq AExp AExp
  deriving(Show)

data CExp = While BExp CExp
  | If BExp CExp CExp
  | Seq CExp CExp
  | Atrib AExp AExp
  | Repeat CExp BExp
  | Do CExp BExp
  | For AExp AExp AExp CExp
  | DuplaAtribuição AExp AExp AExp AExp
  | Swap AExp AExp
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
bbigStep (Leq e1 e2, s) = let(n1,s1) = abigStep (e1, s)
                             (n2,s2) = abigStep (e2, s)
                             in (n1 <= n2, s)

cbigStep :: (CExp,Estado) -> (CExp,Estado)
cbigStep (Skip,s)          = (Skip,s)
cbigStep (If b c1 c2,s) = case bbigStep(b,s) of
                (True, _) -> cbigStep(c1, s)
                (False, _) -> cbigStep(c2, s)

cbigStep (Seq c1 c2,s) = let (com1, s1) = cbigStep(c1, s) in cbigStep(c2, s1)

cbigStep (Atrib (Var x) e,s) = let (n1, s1) = abigStep(e, s) in (Skip, mudaVar s x n1)

cbigStep (While b c, s) = case bbigStep(b, s) of
                (True, _) -> let (loop, s') = cbigStep(c, s) in cbigStep(Seq (c) (While b c), s')
                (False, _) -> (Skip, s)

--swap(x,y)
cbigStep (Swap (Var x) (Var y), s) = (Skip, mudaVar (mudaVar s x (procuraVar s y)) y (procuraVar s x))

-- repeat a until b
cbigStep (Repeat c b, s) = let (c', s') = cbigStep(c, s) in case bbigStep(b, s') of
                (True, _) -> (Skip, s)
                (False, _) -> cbigStep(Seq c (Repeat c' b), s')
--do a while b
cbigStep (Do c b, s) = let (c', s') = cbigStep(c, s) in case bbigStep(b, s') of
                (True, _) -> cbigStep(Seq c (Repeat c' b), s')
                (False, _) -> (Skip, s)

--for  x e1 e2 c     for x from e1 to e2 do c, s
-- if e1 <= e2 then x = e1; c ; for x from (e1+1) to e2 do c else Skip, s ==> <Skip, s'>
-- cbigStep (For (Var x) e1 e2 c, s) = case bbigStep(Leq (e1) (e2)) of
--                                 (True, _) -> let (x, s) = cbigStep(A)
--                                 (False, _) -> (Skip, s)


--duplaAtribuição
cbigStep( DuplaAtribuição (Var x) (Var y) e1 e2, s) = let (n1, s') = abigStep(e1, s) in
                          cbigStep ( Atrib (Var y) (e2), (mudaVar s x n1))



meuEstado :: Estado
meuEstado = [("x",4), ("y",0), ("z",0)]

-- testaLeq :: BExp
-- testaLeq = (Leq (Var "x") (Var "y"))
-- testaLeq2 :: BExp
-- testaLeq2 = (Leq (Var "y") (Var "z"))
-- testaLeq3 :: CExp
-- testaLeq3 = (Seq (Atrib (Var "y")(Num 4))
--                  (If (Leq (Var "x") (Var "y")) (Atrib (Var "z")(Num 99)) (Skip) ))

testewhile :: CExp
testewhile = (While (Not (Ig (Var "x") (Num 1)))
                    (Atrib (Var "x") (Sub (Var "x")(Num 1)))  )

estadoTestaSwap :: Estado
estadoTestaSwap = [("x",3), ("y",2), ("z",0)]

-- estadoTestaDA :: Estado
-- estadoTestaDA = [("x",3), ("y",2), ("z",0)]
--
-- testeDA :: CExp
-- testeDA = DuplaAtribuição (Var "x") (Var "y") (Num 15) (Num 25)

exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

teste1 :: BExp
teste1 = (Ig (Som (Num 3) (Num 3))  (Mul (Num 2) (Num 3)))
teste2 :: BExp
teste2 = (Ig (Som (Var "x") (Num 3))  (Mul (Num 2) (Num 3)))


testec1 :: CExp
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y")))
        (Atrib (Var "y") (Var "z")))

fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Ig (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
-- y = 1
-- while x != 1
--   y = y * x
--   x = x - 1
-- x = 1     y = 24
testaSwap :: CExp
testaSwap = (Swap (Var "x") (Var "y"))
