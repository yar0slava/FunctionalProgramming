{-# OPTIONS_GHC -Wall #-}
module Kurash12 where

import Data.Char(isDigit, digitToInt, intToDigit)
import Data.Maybe(isJust, fromJust)

data Value = I Int  | B Bool deriving (Show, Eq)
data Exp = Var String      -- Çì³ííà
         | Const Value     -- êîíñòàíòà
         | Op Exp Bop Exp  -- Îïåðàö³ÿ
                 deriving (Show, Eq)
-- Á³íàðí³ (2-àðãóìåíòà) îïåðàòîðè
data Bop =  Plus | Minus | Times | Div   
          | Gt | Ge | Lt | Le| Eql | And | Or
            deriving (Show, Eq)

data Stmt = Assign String Exp
          | Read String 
          | Write Exp
          | Incr String
          | If Exp Stmt 
          | While Exp Stmt       
          | For Stmt Exp Stmt Stmt
          | Block [(String,Type)] [Stmt]        
          deriving (Show, Eq)
data Type = It | Bt deriving (Show, Eq)
type Program = Stmt

type StateW = ([String], [(String,Value)], [String])

type VarEnv  = [(String,Type)]

-- Задача 1 -----------------------------------------
getValue::  StateW -> String -> Value
getValue (_,ss,_) name = snd$head$filter (\x -> fst x == name) ss

updValue :: StateW -> String -> Value -> StateW
updValue (inp,ss,out) name val = (inp, updated, out)
  where updated = map (\x -> if fst x == name then (fst x,val) else x) ss

-- Задача 2 ----------------------------------------- 
readValue :: StateW -> Type -> (StateW,Value)
readValue st@(inp,ss,out) It | (not$null inp) && (isNumber$head inp) = ((tail inp,ss,out),(I (toNumber$head inp)))
                             | otherwise = (st,(I 0))

readValue st@(inp,ss,out) Bt | not$null inp && head inp == "False" = ((tail inp,ss,out),(B False))
                             | not$null inp && head inp == "True" = ((tail inp,ss,out),(B True))
                             | otherwise = (st,(B False))

readAddValue :: StateW -> String -> StateW
readAddValue ([],_,_) _ = error "nothing to read"
readAddValue st@(inp,ss,out) s | isNumber$head inp = (tail inp,(s,(I (toNumber$head inp))):ss,out)
                               | head inp == "False" = (tail inp,(s,(B False)):ss,out)
                               | head inp == "True" = (tail inp,(s,(B True)):ss,out)
                               | otherwise = st

isNumber :: String -> Bool
isNumber [] = True
isNumber str | (head str == '-') || (isDigit$head str) = isNumber$tail str
             | otherwise = False

toNumber :: String -> Int
toNumber [] = 0
toNumber str | head str == '-' = -1*(toNumber$tail str)
             | otherwise = ((10^(length str -1))*(digitToInt$head str)) + (toNumber$tail str)


-- Задача 3 -----------------------------------------
writeValue :: StateW -> Value -> StateW 
writeValue (inp,ss,out) (I v) = (inp,ss,out++[toString v])
writeValue (inp,ss,out) (B True) = (inp,ss,out++["True"])
writeValue (inp,ss,out) (B False) = (inp,ss,out++["False"])


toString :: Int -> String
toString 0 = ['0']
toString x | x < 0 = '-':toString(-1*x)
           | div x 10 > 0 = (toString$div x 10)++[intToDigit(mod x 10)]
           | otherwise = [intToDigit(mod x 10)]

-- Задача 4 ----------------------------------------- 
evExp :: StateW -> Exp -> Value
evExp _ (Const v) = v
evExp st (Var s) | null s = (B False)
                 | otherwise = getValue st s
evExp st (Op e1 Plus e2) = intOp (evExp st e1) (evExp st e2) (+)
evExp st (Op e1 Minus e2) = intOp (evExp st e1) (evExp st e2) (-)
evExp st (Op e1 Times e2) = intOp (evExp st e1) (evExp st e2) (*)
evExp st (Op e1 Div e2) = intOp (evExp st e1) (evExp st e2) (div)
evExp st (Op e1 Gt e2) = ibOp (evExp st e1) (evExp st e2) (>)
evExp st (Op e1 Ge e2) = ibOp (evExp st e1) (evExp st e2) (>=)
evExp st (Op e1 Lt e2) = ibOp (evExp st e1) (evExp st e2) (<)
evExp st (Op e1 Le e2) = ibOp (evExp st e1) (evExp st e2) (<=)
evExp st (Op e1 Eql e2) = ibOp (evExp st e1) (evExp st e2) (==)
evExp st (Op e1 And e2) = boolOp (evExp st e1) (evExp st e2) (&&)
evExp st (Op e1 Or e2) = boolOp (evExp st e1) (evExp st e2) (||)

intOp :: Value -> Value -> (Int->Int->Int) -> Value
intOp (I v1) (I v2) f = (I (f v1 v2))
intOp _ _ _ = error "parameters must be integer"

ibOp :: Value -> Value -> (Int->Int->Bool) -> Value
ibOp (I v1) (I v2) f = (B (f v1 v2))
ibOp _ _ _ = error "parameters must be integer"

boolOp :: Value -> Value -> (Bool->Bool->Bool) -> Value
boolOp (B v1) (B v2) f = (B (f v1 v2))
boolOp _ _ _ = error "parameters must be boolean"


-- Задача 5 -----------------------------------------
evStmt :: StateW -> Stmt -> StateW 
evStmt st (Assign s ex) | null s = st 
                        | otherwise = updValue st s (evExp st ex)
evStmt st (Read s) = readAddValue st s

evStmt st (Write ex) = writeValue st (evExp st ex)
evStmt st (Incr s) = case getValue st s of (I int) -> updValue st s (I (int+1))
                                           (B _) -> st
 
evStmt st (If ex stmt) = case evExp st ex of (B True) -> evStmt st stmt
                                             _ -> st

evStmt st while@(While ex stmt) = case evExp st ex of (B True) -> evStmt (evStmt st stmt) while
                                                      _ -> st

evStmt st (For stmt1 ex stmt2 stmt3) = case evExp afterInit ex of (B True) -> evStmt afterUpdate (For nothing ex stmt2 stmt3)
                                                                  _ -> st
        where afterInit = evStmt st stmt1
              afterBody = evStmt afterInit stmt3
              afterUpdate = evStmt afterBody stmt2
              nothing = (Assign [] (Var "")) 

evStmt st (Block vars stmts) = foldl (evStmt) (addVals vars st) stmts

addVals :: [(String,Type)] -> StateW -> StateW
addVals s (inn,ss,out) = (inn,(map func s)++ss,out)

func :: (String,Type) -> (String,Value)
func (s,It) = (s,(I 0))
func (s,Bt) = (s,(B False))


-- Задача 6 -----------------------------------------
evProgram :: Program -> [String] -> [String]
evProgram pr inp = getOut$evStmt (inp,[],[]) pr
    where getOut (_,_,out) = out

-- Задача 7 -----------------------------------------
iswfOp :: Bop -> [Type] -> Maybe Type 
iswfOp bop [It,It] | bop == Plus || bop == Minus ||
                     bop == Times || bop == Div = Just(It)
                   | bop == Gt || bop == Ge ||
                     bop == Lt || bop == Le || bop == Eql = Just(Bt)
                   | otherwise = Nothing
iswfOp bop [Bt,Bt] | bop == And || bop == Or = Just(Bt)
                   | otherwise = Nothing
iswfOp _ _ = Nothing

-- Задача 8 -----------------------------------------
iswfExp :: Exp -> VarEnv -> Maybe Type 
iswfExp (Var name) ve | not$containsVar ve name = Nothing
                      | otherwise = Just(getType ve name)
iswfExp (Const (I _)) _ = Just(It)
iswfExp (Const (B _)) _ = Just(Bt)
iswfExp (Op ex1 bop ex2) ve | isJust ex1Type && isJust ex2Type = 
   iswfOp bop [(fromJust ex1Type),(fromJust ex2Type)]
                            | otherwise = Nothing
   where ex1Type = iswfExp ex1 ve
         ex2Type = iswfExp ex2 ve

getType:: VarEnv -> String -> Type
getType ve name = snd$head$filter (\x -> fst x == name) ve

-- Задача 9 -----------------------------------------
iswfStmt :: Stmt -> VarEnv -> Bool 
iswfStmt (Assign _ ex) ve = case iswfExp ex ve of Just It -> True
                                                  _       -> False
iswfStmt (Read name) ve = containsVar ve name
iswfStmt (Write ex) ve = case iswfExp ex ve of Just It -> True
                                               _       -> False
iswfStmt (Incr name) ve | containsVar ve name  = case iswfExp (Var name) ve of Just It -> True
                                                                               _       -> False
                        | otherwise = False
iswfStmt (If ex stmt) ve | iswfExp ex ve == Just Bt && iswfStmt stmt ve = True
                         | otherwise = False
iswfStmt (While ex stmt) ve | iswfExp ex ve == Just Bt && iswfStmt stmt ve = True
                            | otherwise = False
iswfStmt (For stmt1 ex stmt2 stmt3) ve | iswfStmt stmt1 ve && iswfExp ex ve == Just Bt && 
                                         iswfStmt stmt2 ve && iswfStmt stmt3 ve = True
                                       | otherwise = False
iswfStmt (Block ves stmts) ve = and$[iswfStmt s (ve++ves) | s <- stmts]



containsVar :: VarEnv -> String -> Bool 
containsVar ve name = not$null$filter (\x -> fst x == name) ve

--------------------------------
iswfProgram :: Program -> Bool 
iswfProgram st = iswfStmt st []



-- -------------------------------------------

{- { int b, e, out;  
     read b; read e; out:= 1;
	 if (b>0 & e>0)
	   {int i; for (i:=0; i<e; i++) out := out*b}; 
     write out  
   }
-}
power :: Program
power = Block [("b",It),("e",It),("out",It)]
              [ Read "b", Read "e", Assign "out" (Const(I 1))
              , If (Op (Op (Var "b") Gt (Const(I 0)))
                       And
                       (Op (Var "e") Gt (Const(I 0))) )
                   (Block [("i",It)]
                        [For (Assign "i" (Const(I 0))) (Op (Var "i") Lt (Var "e")) (Incr "i")
                              (Assign "out" (Op (Var "out") Times (Var "b")))
                        ]
                   )
              , Write (Var "out") 
              ]


st3 :: StateW
st3 = (["True"],[("i",I 0),("out",I 0)],[])

b1,b2 :: Program
b1 = (Block [("i",It),("out",It)] [For (Assign "i" (Const(I 1))) (Op (Var "i") Lt (Const(I 4))) (Incr "i") (Assign "out" (Op (Var "out") Plus (Var "i")))])
b2 = (Block [("i",It),("out",It)] [(Assign "i" (Const(I 1))), (Assign "out" (Const(I 7))), (Assign "out" (Op (Var "out") Times (Var "i")))])

b3,b4,b5 :: Stmt
b3 = (For (Assign "i" (Const(I 1))) (Op (Var "i") Lt (Const(I 4))) (Incr "i") (Assign "out" (Var "i")))
b4 = (If (Op (Var "i") Gt (Const(I 4))) (Assign "out" (Var "i")))
b5 = (While (Op (Var "i") Le (Const(I 4))) (Block [] [(Assign "out" (Op (Var "out") Plus (Var "i"))), (Incr "i")]))
{- { int a, b; 
     read a; b := 0;
	 if (a>=0)
	   {bool c; c:=true; 
	    while(c) {b++; c:= a >= b*b}
	   };
     write (b-1)
   } 	
-} 
squareRoot :: Program
squareRoot = Block [("a",It),("b",It)]
                   [ Read "a", Assign "b" (Const (I 0))
                   , If (Op (Var "b") Ge (Const(I 0)))
                        (Block [("c", Bt)] 
                               [Assign "c" (Const (B True)),
                                While (Var "c")
                                 (Block []
                                   [(Incr "b"), 
                                    Assign "c" (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                                   ])
                               ]
                        )
                   , Write (Op (Var "b") Minus (Const (I 1)))
                   ]

{- {int in, out; 
    read in; out := 0; 
	if (in>=0) 
      {int f0, f1, c; 
	   f0 := 1; f1 := 1; out := 1;
       if (in > 1)  
         for (c := 1; c < in; c++) 
		   {out := f0 + f1; f0 := f1; f1 := out}
	 };
    write out	 
  }
-}
fibonacci :: Program
fibonacci = 
    Block [("in",It), ("out",It)]
          [ Read "in",  Assign "out" (Const (I 0))
          , If (Op (Var "in") Gt (Const(I 1))) 
               (Block [("f0",It), ("f1",It), ("c",It)]
                     [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)),
                      If (Op (Var "in") Gt (Const (I 1)))
                         (For (Assign "c" (Const (I 1)))
                             (Op (Var "c") Lt (Var "in")) 
                             (Incr "c")
                             (Block []
                                    [Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                    , Assign "f0" (Var "f1")
                                    , Assign "f1" (Var "out")
                                    ]
                              )
                         )
                     ])
          , Write (Var "out")
          ]



