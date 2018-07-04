prog1 :: Exp
prog1 = Ap (IF TRUE (Fun "x" INT (Soma (Var "x") (Num 1))) (Fun "x" INT (Soma (Var "x") (Num 2)))) (Num 2)

-- > (if True then (Fun x:Int in x + 1) (Fun x:Int in x+2) 2 
-- Resp: 3


prog2 :: Exp
prog2 = (Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Ap (Var "x") (Num 10)))

-- > (Let x: Int -> Int = (fun x : Int => x + 1) in x 10
-- Resp: 11




prog3 :: Exp
prog3 = Fun "f1" (F INT INT) (Fun "y" BOOL (Soma (Num 4) (Ap (Var "f1") (Num 1))))

-- > fun f1 : Int -> Int => (fun y: Bool => 4 + (f1 1))

prog4 :: Exp
prog4 = Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Var "x") 

-- > Let x: Int -> Int = (fun x : Int => x + 1) in x 


erroTipo1 :: Exp
erroTipo1 = IF TRUE (Fun "x" INT (Soma (Var "x") (Num 1))) (Num 3)


erroTipo2 :: Exp
erroTipo2 = (Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Ap (Var "x") TRUE))
  

-- > Let x: Int -> Int = (fun x : Int => x + 1) in x True 



