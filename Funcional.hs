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
bigStep (Var "x") = (Var "x")
bigStep (Fun x t e) = (Fun x t e)

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


--bigStep (IF b (Num n1) (Num n2)) = case bigStep b of 
--						TRUE -> (Num n1)
--						FALSE -> (Num n2)

--bigStep (IF b TRUE FALSE) = case bigStep b of 
--						TRUE -> TRUE
--						FALSE -> FALSE

--bigStep (IF b TRUE TRUE) = case bigStep b of 
--						TRUE -> TRUE
--						FALSE -> TRUE

--bigStep (IF b FALSE FALSE) = case bigStep b of 
--						TRUE -> FALSE
--						FALSE -> FALSE

--bigStep (IF b FALSE TRUE) = case bigStep b of 
--						TRUE -> FALSE
--						FALSE -> TRUE

bigStep (IF b e1 e2) = if (final (bigStep e1) (bigStep e2)) 
						then case bigStep b of 
							TRUE -> bigStep e1
							FALSE -> bigStep e2
						else error "Tipos diferentes"

--bigStep (IF b (Fun x1 t1 e1) (Fun x2 t2 e2)) = case bigStep b of 
--												TRUE -> (Fun x1 t1 e1)
--												FALSE -> (Fun x2 t2 e2)

--bigStep (IF b e1 e2) =  bigStep (IF b (bigStep e1) (bigStep e2))

bigStep (Ap (Fun x _ e1) e2) = bigStep (subs x (bigStep e2) e1)
bigStep (Ap e1 e2) = bigStep (Ap (bigStep e1) e2)
--bigStep (Ap (Fun x BOOL e1) e2) = bigStep (subs x (bigStep e2) e1)

--bigStep (Fun x INT e) = subs x (bigStep e) e
--bigStep (Fun x BOOL e) = subs x (bigStep e) e

bigStep (Let x _ e1 e2) = bigStep (subs x (bigStep e1) e2)
--bigStep (Let x BOOL e1 e2) = bigStep (subs x (bigStep e1) e2)
--bigStep (Let x (F t1 t2) e1 e2) = bigStep (subsFun x (bigStep e1) e2)
-- bigStep ? = ?

--bigStep (Fun "y" INT (Soma (Var "y") (Num 4)) (Num 6))

final:: Exp->Exp->Bool
final TRUE TRUE = True
final FALSE TRUE = True
final TRUE FALSE = True
final FALSE FALSE = True
final (Num _) (Num _) = True
final (Fun _ _ _) (Fun _ _ _) = True
final _ _ = False

prog6:: Exp
prog6 = Ap (Fun "x" INT (Soma (Var "x") (Num 3))) (Num 4)

prog7:: Exp
prog7 = Ap (Fun "x" BOOL (IF ((Var "x")) (Num 4) (Num 6))) (FALSE)

prog8:: Exp
prog8 = Let "z" INT (Soma (Num 4) (Num 2)) (Var "z")

prog9:: Exp
prog9 = Ap (Fun "y" INT (Let "z" INT (Soma (Num 4) (Var "y")) (Var "z"))) (Num 2)

prog10:: Exp
--prog10 = Ap (Fun "x" INT (Ap (Fun "y" INT (Let "z" INT (Soma (Var "x") (Var "y")) (Var "z"))) (Num 2))) (Num 4)
prog10 = Ap (Fun "x" INT (Ap (Fun "y" INT (Let "z" INT (Soma (Var "x") (Var "y")) (Var "z"))) (Num 2))) (Num 4)

--prog8:: Exp
--prog8 = Ap (Fun "x" INT (Ap (Fun "y" INT (Let "z" INT (Soma (Var "x") (Var "y")) (Var "z"))) (Num 2)) (Num 4))

subs :: String -> Exp -> Exp -> Exp
-- {v/x}e1 = subs x v e1
subs var val (Soma exp1 exp2) = Soma (subs var val exp1) (subs var val exp2)
subs var val (Mult exp1 exp2) = Mult (subs var val exp1) (subs var val exp2)
subs var val (And exp1 exp2) = And (subs var val exp1) (subs var val exp2)
subs var val (Or exp1 exp2) = Or (subs var val exp1) (subs var val exp2)
subs var val (Not exp1) = Not (subs var val exp1)
subs var val (IF b exp1 exp2) = IF (subs var val b) (subs var val exp1) (subs var val exp2)
subs var val (Let x t exp1 exp2) = Let x t (subs var val exp1) (subs var val exp2)
subs var val (Ap exp1 exp2) = Ap (subs var val exp1) (subs var val exp2)
subs var val (Fun x t exp1) = Fun x t (subs var val exp1)

subs x y (Var a)
    | a == x    = y
    | otherwise = (Var a)

subs x y (Num d) = (Num d)
subs x y TRUE = TRUE
subs x y FALSE = FALSE


--subs :: String -> Exp -> Exp -> Exp
---- {v/x}e1 = subs x v e1
--subs var val (And exp1 exp2) = And (subs var val exp1) (subs var val exp2)
--subs var val (Or exp1 exp2) = Or (subs var val exp1) (subs var val exp2)
--subs var val (Not exp1) = Not (subs var val exp1)

--subs x y (Var a)
--    | a == x    = y
--    | otherwise = (Var a)

--subs x y TRUE = TRUE
--subs x y FALSE = FALSE
----subs x y (Num s) = (Num s)


-- Let "x" INT (Soma (Num 3) (Num 4)) (Mult (Var "x") (Num 3))

--subs x (Soma (Num 3) (Num 4)) (Soma (Var "x") (Num 3)) = Soma (subs x (Soma (Num 3) (Num 4)) ((Var "x"))) (subs x (Soma (Num 3) (Num 4)) ((Num 3)))
 
--prog1 :: Exp
--prog1 = Ap (IF TRUE (Fun "x" INT (Soma (Var "x") (Num 1))) (Fun "x" INT (Soma (Var "x") (Num 2)))) (Num 2)

-- > (if True then (Fun x:Int in x + 1) (Fun x:Int in x+2) 2 
-- Resp: 3


--prog2 :: Exp
--prog2 = (Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Ap (Var "x") (Num 10)))

-- > (Let x: Int -> Int = (fun x : Int => x + 1) in x 10
-- Resp: 11

--prog3 :: Exp
--prog3 = Soma (Num 3) (Num 4)

--prog4 :: Exp
--prog4 = (And (Or FALSE TRUE) FALSE)

prog5 :: Exp
prog5 = IF (And (Or FALSE TRUE) FALSE) (Soma (Num 3) (Num 4)) (Mult (Num 9) (Num 4))



prog1 :: Exp
prog1 = Ap (IF TRUE (Fun "x" INT (Soma (Var "x") (Num 1))) (Fun "x" INT (Soma (Var "x") (Num 2)))) (Num 2)

-- > (if True then (Fun x:Int in x + 1) (Fun x:Int in x+2) 2 
-- Resp: 3


prog2 :: Exp
prog2 = (Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Ap (Var "x") (Num 10)))

-- > (Let x: Int -> Int = (fun x : Int => x + 1) in x 10
-- Resp: 11




prog3 :: Exp
--prog3 = Fun "f1" (F INT INT) (Fun "y" BOOL (Soma (Num 4) (Ap (Var "f1") (Num 1))) )
prog3 = Ap (Fun "f1" (F INT INT) (Soma (Num 4) (Ap (Var "f1") (Num 1)))) (Fun "z" INT (Soma (Var "z") (Num 1)))
-- > fun f1 : Int -> Int => (fun y: Bool => 4 + (f1 1))
-- > 6


prog4 :: Exp
prog4 = Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Var "x") 

-- > Let x: Int -> Int = (fun x : Int => x + 1) in x 
-- > Fun x:Int => x + 1


erroTipo1 :: Exp
erroTipo1 = IF TRUE (Fun "x" INT (Soma (Var "x") (Num 1))) (Num 3)


erroTipo2 :: Exp
erroTipo2 = (Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Ap (Var "x") TRUE))
  

-- > Let x: Int -> Int = (fun x : Int => x + 1) in x True 




