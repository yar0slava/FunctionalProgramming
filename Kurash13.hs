{-# OPTIONS_GHC -Wall #-}
module Kurash13 where

import Text.ParserCombinators.Parsec

-- Задача 1 -----------------------------------------
fullBrace :: Parser()
fullBrace = spaces >> full >> eof
    where full = parenth <|> brack <|> bracess <|> spaces
          parenth = symbol '(' >> spaces >> full >> symbol ')' >> spaces >> full
          brack = symbol '[' >> spaces >> full >> symbol ']' >> spaces >> full
          bracess = symbol '{' >> spaces >> full >> symbol '}' >> spaces >> full

balance  :: String -> Bool
balance str = either (const False) (const True) (parse fullBrace "" str)
   
-- Задача 2 ----------------------------------------- 
data Bexp = Bvalue Bool | Bvar Char | Not Bexp 
          | And Bexp Bexp | Or Bexp Bexp  deriving (Eq, Show)  

fullBe :: Parser Bexp 
fullBe = bexp <* eof

bdis :: Parser Bexp
bdis = symbol '(' *> bexp <* symbol ')'
    <|> do {symbol '!'; Not <$> bdis}
    <|> Bvar <$> letter
    <|> do {_ <- string "true"; 
            return $ Bvalue True}
    <|> do {_ <- string "false"; 
            return $ Bvalue False}

bcon :: Parser Bexp
bcon = chainl1 bdis (do {_ <- symbol '&'; 
                         return And})

bexp :: Parser Bexp
bexp = chainl1 bcon (do {_ <- symbol '|'; 
                         return Or})

anBexp :: String -> Maybe Bexp
anBexp str = case (parse fullBe "" str) of
                Left _   ->  Nothing
                Right ex -> Just ex

-- Задача 3 ----------------------------------------- 
type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

text :: Parser XML
text = Text <$> (many1 $ noneOf "<>")

value :: Parser String
value = many $ noneOf "\""

fullValue :: Parser String
fullValue = string "\"" *> value <* string "\""

name :: Parser Name
name = do {l <- letter; 
           ls <- many(digit <|> letter <|> oneOf ".-"); 
           return (l:ls)}

element :: Parser XML
element = do {_ <- string "<"; 
              n <- name; 
              a <- try $ attrib;
              _ <- string ">";
              x <- try $ many xml; 
              _ <- string "</"; 
              _ <- string n; 
              _ <- string ">";
              return $ Element n a x}

attrib :: Parser Attributes
attrib = many $ do {spaces; 
                    n <- name; 
                    spaces; 
                    _ <- string "="; 
                    spaces; 
                    v <- fullValue; 
                    return (n,v)}

xml :: Parser XML
xml = try (element <|> text)

fullXML :: Parser XML 
fullXML = spaces *> element <* spaces <* eof

anXML :: String -> Maybe XML
anXML str = case (parse fullXML "" str) of
               Left _    -> Nothing
               Right res -> Just res

----------------  Мова SPL  ------------   
data Value = I Int  | B Bool deriving (Show, Eq)
data Exp = Var String
         | Const Value
         | Op Exp Bop Exp
                 deriving (Show, Eq)

-- Бінарні (2-аргумента) оператори
data Bop =  Plus | Minus | Times | Div   
          | Gt | Ge | Lt | Le| Eql | Ba | Bo
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

{- лексика  
  symbol = ';' | '{' | '}' | '(' | ')' 
  identif=  char {digit | char}
  keyword= "int" | "bool" | "read" | "write" |"if" | "while" | "for" | "true" | "false"
  iden   =  identif .... not "int" "bool" "read" ""write" "if" "while" "for" "true" "false"
  number = digit { digit }.
  mulOp  = "*" | "/".
  addOp  = "+" | "-".
  relOp  = "<" | "<=" | ">" | ">=" | "==" 
  disOp  = "&" 
  conOp  = "|"
  typev  = "int" | "bool" 
-}
iden :: Parser String
iden = try( do {nm <- identif;
                if (any(nm==) ["int","bool","read","write","if","while","for","true","false"])
                    then unexpected ("reserved word " ++ show nm)
                    else return nm 
               } ) 

oper  :: String -> Bop -> Parser Bop
oper str bop = do {_ <- string str; return bop}

mulOp :: Parser Bop   
mulOp = (oper "*" Times) <|> (oper "/" Div)

disOp :: Parser Bop   
disOp = (oper "&" Ba)

conOp :: Parser Bop   
conOp = (oper "|" Bo)

-- розпізнати всі "порожні" символи в кінці			
lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

--   :type Op -----> Exp -> Bop -> Exp -> Exp 
--   :type flip Op -------> Bop -> Exp -> Exp -> Exp         
expOp :: Parser Bop -> Parser (Exp -> Exp -> Exp)
expOp p = do {x <- lexem p; return (flip Op x)}

symbol :: Char ->  Parser ()
symbol ch = lexem (char ch >> return ())

keyword :: String -> Parser ()
keyword st = try( lexem( string st >> notFollowedBy alphaNum)) 

typev :: Parser Type 
typev = do {keyword "int"; return It}
        <|> do {keyword "bool"; return Bt} 

-- Задача 4 -----------------------------------------
identif :: Parser String
identif = do {l <- letter; 
              ls <- many (digit <|> letter); 
              return (l:ls)}

number :: Parser Int
number = read <$> (many1 digit)
 
addOp :: Parser Bop  
addOp = oper "+" Plus <|> oper "-" Minus

relOp :: Parser Bop  
relOp = try (oper ">=" Ge) <|> (oper ">" Gt) <|> (oper "==" Eql)
    <|> try (oper "<=" Le) <|> (oper "<" Lt)

{- вирази  
  factor = '(' expr ')' | number | "true" | "false" | iden
  term   = factor { mulOp factor }
  relat  = term { addOp term }
  conj   = relat [relOp relat] 
  disj   = conj { conOp conj}   
  expr   = disj { disOp disj}
-}
factor :: Parser Exp
factor = do { symbol '('; x <- expr; symbol ')'; return x}
     <|> do {nm <- lexem number; return (Const (I nm))}
     <|> do {keyword "true"; return (Const (B True))}
     <|> do {keyword "false"; return (Const (B False))}
     <|> do {cs <- lexem iden; return (Var cs) }
     <?> "factor"

-- Задача 5 -----------------------------------------
term :: Parser Exp     
term = chainl1 factor$expOp mulOp

relat :: Parser Exp
relat = chainl1 term$expOp addOp

conj :: Parser Exp
conj = chainl1 relat$expOp relOp

disj :: Parser Exp
disj = chainl1 conj$expOp conOp

expr :: Parser Exp
expr = chainl1 disj$expOp disOp

{- оператори
  stmt   = "for" forSt | "while" whileSt | "if" ifSt 
         | "read" inSt | "write" outSt | iden assSt | blockSt  
  forSt  = '(' stmt ';' expr ';' stmt ')' stmt 
  whileSt= '(' expr ')' stmt 
  ifSt   = '(' expr ')' stmt 
  inSt   = iden 
  outSt  = expr
  assSt  = "++" | ":=" expr 
  blockSt= '{' {defin} listSt '}' 
  defin  = type listId ';'
  listId = iden {',' iden}
  listSt = stmt {';' stmt}  
  program= stmt eos 
-}   
stmt :: Parser Stmt 
stmt = do {keyword "for"; forSt}
       <|> do {keyword "while"; whileSt}
       <|> do {keyword "if"; ifSt}
       <|> do {keyword "read"; inSt}
       <|> do {keyword "write"; outSt}
       <|> do {var <- lexem iden; assignSt var}
       <|> blockSt
       <?> "statement"

-- Задача 6 -----------------------------------------
forSt :: Parser Stmt  
forSt = do {symbol '('; 
            s1 <- lexem stmt; symbol ';';
            ex <- lexem expr; symbol ';'; 
            s2 <- lexem stmt; 
            symbol ')';
            s3 <- lexem blockSt; 
            return $ For s1 ex s2 s3}

whileSt :: Parser Stmt               
whileSt = do {symbol '('; 
              ex <- lexem expr; 
              symbol ')';
              s <- lexem blockSt; 
              return $ While ex s} 
              
ifSt :: Parser Stmt              
ifSt = do {symbol '('; 
           ex <- lexem expr; 
           symbol ')'; 
           s <- lexem blockSt;
           return $ If ex s} 

inSt :: Parser Stmt              
inSt = do {i <- lexem iden;
           return $ Read i}  

outSt :: Parser Stmt              
outSt = do {ex <- lexem expr;
           return $ Write ex}  

assignSt :: String -> Parser Stmt 
assignSt nm = do {_ <- lexem $ string "++"; 
                  return $ Incr nm} 
          <|> do {_ <- lexem $ string ":="; 
                  ex <- lexem expr; 
                  return $ Assign nm ex}

blockSt :: Parser Stmt
blockSt = do {symbol '{'; 
              d <- try (lexem listDefin); 
              l <- lexem listSt; 
              symbol '}'; 
              return $ Block d l}


listDefin :: Parser [(String,Type)]
listDefin = do {def <- lexem defin;
                defs <- many (do {lexem defin;});
                return (def++(if defs == [[]] then [] else foldl (++) [] defs))};

defin :: Parser [(String,Type)]
defin = do {t <- lexem typev;
            ids <- listId;
            symbol ';';
            return  [(i,t) | i <- ids]}

listSt :: Parser [Stmt]
listSt = do {s <- lexem stmt; 
             sts <- many (do {symbol ';'; 
                              lexem stmt;});
             return (s:sts)}

listId :: Parser [String]
listId = do {i <- lexem iden; 
             is <- many (do {symbol ','; 
                             lexem iden;});
             return (i:is)}
               
---------------------------------------------	
-- Головні функції
---------------------------------------------				
program :: Parser Stmt 
program = do {spaces; r <- stmt; eof; return r}

parseSPL :: String -> Either ParseError Program
parseSPL s = parse program "" s

---------------------------------------------
--- Дані для тестування
--------------------------------------------- 
casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

squareRoot :: String
squareRoot =
   "{int a, b; \
   \ read a; b := 0; \
   \ if (a>= 0)\
   \    {bool c; c:=true; while(c) {b++; c:= a >= b*b}\
   \    };\
   \  write (b-1)\
   \ }"

test :: String
test =
   "{int a, b; \
   \ read a; b := 0; \
   \ if (a>= 0)\
   \    {bool c; c:=true;\
   \  write (b-1)\
   \ }}"



squareRootAST :: Program
squareRootAST = Block [("a",It),("b",It)]
                   [ Read "a", Assign "b" (Const (I 0))
                   , If (Op (Var "a") Ge (Const(I 0))) 
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

fibonacci :: String
fibonacci = 
 " {int in, out; read in; out := 0; \n\
   \if (in>=0){int f0, f1,c; \n\
   \           f0 := 1; f1 := 1; out := 1; \n\
   \           if(in>1) \n \
   \              for (c := 1; c < in; c++) {\n\
   \                   out := f0 + f1; f0 := f1; f1 := out\n\
   \              }\n\
   \          }; \n\
   \write out \n\
  \}"
  
fibonacciAST :: Program
fibonacciAST = 
    Block [("in",It), ("out",It)]
          [ Read "in",  Assign "out" (Const (I 0))
          , If (Op (Var "in") Ge (Const(I 0))) 
               (Block [("f0",It), ("f1",It), ("c",It)]
                           [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)),
                            Assign "out" (Const (I 1)),
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
                            ]
                )
          , Write (Var "out")
          ]

