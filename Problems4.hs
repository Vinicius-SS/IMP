--Dupla: Vinicius Sesti, Henry Nunes 
-- prog01 faz o fatorial de um numero definido como o primeiro LIT do programa 6 por default. "output" é o resultado. Rodar em haskell prog01 "output" 
-- prog02 faz a multiplicação utilizando soma, as variaveis são os dois primeiros LIT do programa, valores 5 e 5 por default. "output" é o resultado. Rodar em haskell prog02 "output" 
-- prog03 faz a Divisão de inteiros utilizando subtração, as variaveis são os dois primeiros LIT do programa, valores 13 e 4 por default. "output" é o resultado. Rodar em haskell prog03 "output" 



type Env = String -> Int

showgambi::Env->String->Int
showgambi e s = e s

env01::Env
env01 "input" = 0
env01 "output" = 1
env01 _ = 0

prog01 = c_eval (SEQ (Assignment "input" (Lit 6))
	(WHILE( Not (IMP_EQ( Lit 1)(Loc "input"))) 
		(
			SEQ (Assignment "output" (Mul(Loc "output")(Loc "input")))
		 	    (Assignment "input"  (Sub(Loc "input" )(Lit 1      )))
		 	)
		)
	)env01

env02::Env
env02 "X" = 0
env02 "Y" = 0
env02 "Output" = 0
env02 _ = 0

prog02 = c_eval (SEQ (Assignment "X" (Lit 5))(SEQ (Assignment "Y" (Lit 5))(WHILE (Not (IMP_EQ (Loc "X") (Lit 0))) (SEQ (Assignment "Output" (Add (Loc "Output")(Loc "Y")))(Assignment "X" (Sub(Loc "X")(Lit 1))))))) env02

env03::Env
env03 "X" = 0
env03 "Y" = 0
env03 "Output" = 0
env03 _ = 0

prog03 = c_eval (SEQ (Assignment "X" (Lit 13))(SEQ (Assignment "Y" (Lit 4))(WHILE (IMP_LEQ (Loc "Y")(Loc "X")) (SEQ (Assignment "X" (Sub (Loc "X")(Loc "Y")))(Assignment "Output" (Add (Loc "Output")(Lit 1))))))) env03




env04::Env
env04 "X" = 0
env04 "Y" = 0
env04 "R1" = 0
env04 "R2" = 0
env04 "Output" = 0
env04 _ = 0

--prog04 = c_eval (SEQ (Assignment "X" (Lit 6))(SEQ (Assignment "X" (Lit 4))())) env04






updt::(Env,String, Int) -> Env
updt(e, s, i) = (\s'-> if s==s' then i else e s')

data AExp = Lit Int | Loc String | Add AExp AExp | Sub AExp AExp | Mul AExp AExp

a_eval::AExp->Env->Int
a_eval (Lit n) env = n
a_eval (Loc s) env = env s
a_eval (Add exp1 exp2) env = (a_eval exp1 env) + (a_eval exp2 env)
a_eval (Sub exp1 exp2) env = (a_eval exp1 env) - (a_eval exp2 env)
a_eval (Mul exp1 exp2) env = (a_eval exp1 env) * (a_eval exp2 env)

data BExp = LitBool Bool | IMP_EQ AExp AExp | IMP_LEQ AExp AExp | Not BExp | AND BExp BExp
b_eval::BExp->Env->Bool
b_eval (LitBool b) env = b
b_eval (IMP_EQ exp1 exp2) env = (a_eval exp1 env) == (a_eval exp2 env)
b_eval (IMP_LEQ exp1 exp2) env = (a_eval exp1 env) <= (a_eval exp2 env)
b_eval (Not exp1) env = not (b_eval exp1 env)
b_eval (AND exp1 exp2) env = (b_eval exp1 env) && (b_eval exp2 env)

data Command = Skip | Assignment String AExp | SEQ Command Command | IFTHENELSE BExp Command Command
					| WHILE BExp Command | REPEATUNTIL Command BExp

c_eval::Command->Env->Env
c_eval Skip env = env
c_eval (Assignment s exp1) env = updt (env, s, (a_eval exp1 env))
c_eval (SEQ com1 com2) env = c_eval com2 (c_eval com1 env)
c_eval (IFTHENELSE bexp comTrue comFalse) env =
	if b_eval bexp env then c_eval comTrue env else c_eval comFalse env
c_eval (WHILE bexp com) env = 
	if b_eval bexp env then c_eval (SEQ com (WHILE bexp com)) env else env
c_eval (REPEATUNTIL com bexp) env = 
	if not (b_eval bexp env) then c_eval (REPEATUNTIL com bexp) env else env

