data Exp = Num Int | TRUE | FALSE | Var String | Soma Exp Exp
	| Mult Exp Exp | And Exp Exp | Or Exp Exp | Not Exp | IF Exp Exp Exp
	| Ap Exp Exp | Fun String Tipo Exp | Let String Tipo Exp Exp
	deriving (Eq,Show)
data Tipo = INT | BOOL | F Tipo Tipo
	deriving (Eq, Show)


bigStep :: Exp -> Exp
bigStep (Num n) = Num n
bigStep TRUE = TRUE
bigStep FALSE = FALSE

bigStep (Soma e1 e2)  = let	(Num n1) = bigStep e1
				(Num n2) = bigStep e2
					in Num (n1+n2)

bigStep (Mult e1 e2)  = let	(Num n1) = bigStep e1
				(Num n2)= bigStep e2
					in Num (n1*n2)

bigStep (Not e) = case bigStep e of
					  TRUE -> FALSE
					  FALSE -> TRUE

bigStep (And e1 e2) = case bigStep e1 of
					  FALSE -> FALSE
					  TRUE -> bigStep e2

bigStep (Or e1 e2) = case bigStep e1 of
					  TRUE -> TRUE
					  FALSE -> bigStep e2

bigStep (IF b e1 e2) =  case bigStep b of 
						TRUE -> bigStep e1
						FALSE -> bigStep e2

--bigStep (Ap (Fun x t e1) e2) = bigStep (subs x e2 e1)
bigStep (Let x t e1 e2) = bigStep (subs x (bigStep e1) e2)
-- bigStep ? = ?

prog6:: Exp
prog6 = Ap (Fun "x" INT (Soma (Var "x") (Num 3))) (Num 4)

subs :: String -> Exp -> Exp -> Exp
-- {v/x}e1 = subs x v e1
subs var val (Soma exp1 exp2) = Soma (subs var val exp1) (subs var val exp2)

subs x y (Var a)
    | a == x    = y
    | otherwise = (Var a)

subs x y (Num d) = (Num d)
 
prog1 :: Exp
prog1 = Ap (IF TRUE (Fun "x" INT (Soma (Var "x") (Num 1))) (Fun "x" INT (Soma (Var "x") (Num 2)))) (Num 2)

-- > (if True then (Fun x:Int in x + 1) (Fun x:Int in x+2) 2 
-- Resp: 3


prog2 :: Exp
prog2 = (Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Ap (Var "x") (Num 10)))

-- > (Let x: Int -> Int = (fun x : Int => x + 1) in x 10
-- Resp: 11

prog3 :: Exp
prog3 = Soma (Num 3) (Num 4)

prog4 :: Exp
prog4 = (And (Or FALSE TRUE) FALSE)

prog5 :: Exp
prog5 = IF (And (Or FALSE TRUE) FALSE) (Soma (Num 3) (Num 4)) (Mult (Num 9) (Num 4))