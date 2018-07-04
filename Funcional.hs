data Exp = Num Int | TRUE | FALSE | Var String | Soma Exp Exp
	| Mult Exp Exp | And Exp Exp | Or Exp Exp | Not Exp | IF Exp Exp Exp
	| Ap Exp Exp | Fun String Tipo Exp | Let String Tipo Exp Exp
	deriving (Eq,Show)
data Tipo = INT | BOOL | F Tipo Tipo
	deriving (Eq, Show)


bigStep :: Exp -> Exp
bigStep (Soma e1 e2)  = let	(Num n1)= bigStep e1
				(Num n2)= bigStep e2
					in Num (n1+n2)
-- bigStep ? = ?


subs :: String -> Exp -> Exp -> Exp
subs var val (Soma exp1 exp2) = Soma (subs var val exp1) (subs var val exp2)
subs var val ? = ?
-- {v/x}e1 = subs x v e1
 
prog1 :: Exp
prog1 = Ap (IF TRUE (Fun "x" INT (Soma (Var "x") (Num 1))) (Fun "x" INT (Soma (Var "x") (Num 2)))) (Num 2)

-- > (if True then (Fun x:Int in x + 1) (Fun x:Int in x+2) 2 
-- Resp: 3


prog2 :: Exp
prog2 = (Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Ap (Var "x") (Num 10)))

-- > (Let x: Int -> Int = (fun x : Int => x + 1) in x 10
-- Resp: 11


