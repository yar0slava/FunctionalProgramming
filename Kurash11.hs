{-# OPTIONS_GHC -Wall #-}
module Kurash11 where

import Data.Char(isLower)
import Data.Maybe(isJust, fromJust)

data BExp = Bvalue Bool | Bvar Char | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Char, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Char, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- Задача 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat (i,xs) ys | null xs = i /= 0
                   | otherwise = res /= 0 
   where res = fst$until cond1 (step1 ys) (i,xs)

cond1 :: (Int,[BDDNode]) -> Bool
cond1 (i,_) = i==0 || i==1

step1 :: Env -> (Int,[BDDNode]) -> (Int,[BDDNode])
step1 xs (i,ys) | val = (getSecond node, ys)
                | otherwise = (getFirst node, ys)
    where node = findNode i ys
          nodeChar = getCh node
          val = findVal xs nodeChar


findVal :: Env -> Char -> Bool
findVal xs ch = head[snd x | x <- xs, fst x == ch]

findNode :: Int -> [BDDNode] -> BDDNode
findNode i xs = head[x | x <- xs, fst x == i]

getCh :: BDDNode -> Char
getCh (_,(ch,_,_)) = ch

getFirst :: BDDNode -> Int
getFirst (_,(_,f,_)) = f

getSecond :: BDDNode -> Int
getSecond (_,(_,_,s)) = s

-- Задача 2 -----------------------------------------
sat :: BDD -> [[(Char, Bool)]]
sat bdd@(_,xs) | null xs = []
               | otherwise = [x | x <- getAllVals$getAllChars bdd, checkSat bdd x]

getAllVals :: [Char] -> [Env]
getAllVals [] = []
getAllVals [x] = [[(x,True)],[(x,False)]]
getAllVals (x:xs) = [(x,b):y | y <- getAllVals xs, b <- [True,False]]

getAllChars :: BDD -> [Char]
getAllChars (_,xs) | null xs = []
                   | otherwise = removeDublicates[getCh x | x <- xs]

removeDublicates :: [Char] -> [Char]
removeDublicates [] = []
removeDublicates (x:xs) | elem x xs = removeDublicates xs
                        | otherwise = x:(removeDublicates xs)

-- Задача 3 -----------------------------------------
simplify :: BExp -> BExp
simplify (Not(Bvalue bool)) = Bvalue(not bool)
simplify (And (Bvalue bool1) (Bvalue bool2)) = Bvalue(bool1 && bool2)
simplify (Or (Bvalue bool1) (Bvalue bool2)) = Bvalue(bool1 || bool2)
simplify e = e

-- Задача 4 -----------------------------------------
restrict :: BExp -> Char -> Bool -> BExp
restrict e@(Bvalue _) _ _ = e
restrict e@(Bvar ch) x v | ch == x = Bvalue v
                         | otherwise = e
restrict (Not e1) x v  = simplify(Not(restrict e1 x v))
restrict (And e1 e2) x v = simplify(And (restrict e1 x v) (restrict e2 x v))
restrict (Or e1 e2) x v = simplify(Or (restrict e1 x v) (restrict e2 x v))

-- Задача 5 -----------------------------------------
-- Передумова: Кожна змінна (індекс) в бульовому виразі (BExp) з"являється 
--    точно один раз в списку індексів (Index); немає інших елементів
buildBDD :: BExp -> [Char] -> BDD
buildBDD e xs = buildBDD' e 2 xs 

buildBDD' :: BExp -> NodeId -> [Char] -> BDD
buildBDD' (Bvalue bool) _ [] | bool = (1,[])
                             | otherwise = (0,[])

buildBDD' e i [x] = (i,[(i,(x,fst leftBDD, fst rightBDD))])
       where leftBDD = buildBDD' (restrict e x False) (i*2) []
             rightBDD = buildBDD' (restrict e x True) (i*2+1) []

buildBDD' e i xs  = (i,currNode:(leftNodes++rightNodes))
       where leftBDD = buildBDD' (restrict e (head xs) False) (i*2) (tail xs)
             rightBDD = buildBDD' (restrict e (head xs) True) (i*2+1) (tail xs)
             currNode = (i,(head xs, i*2, i*2+1))
             leftNodes = snd leftBDD
             rightNodes = snd rightBDD

-- Задача 6 -----------------------------------------
-- Передумова: Кожна змінна (індекс) в бульовому виразі (BExp) з"являється 
--    точно один раз в списку індексів (Index); немає інших елементів
buildROBDD :: BExp -> [Char] -> BDD
buildROBDD = undefined

-- Задача 7 -----------------------------------------
fullBexp :: String -> Maybe BExp 
fullBexp s | isJust(bexp s) && snd getExpr == [] = Just(fst getExpr)
           | otherwise = Nothing

     where getExpr = fromJust$bexp s

bexp :: String -> Maybe (BExp,String)
bexp s | isJust(bcon s) && isJust getManyCon1 = getManyCon1
       | otherwise = Nothing
     
     where getManyCon1 = manyCon$fromJust$bcon s

bcon :: String -> Maybe (BExp,String)
bcon s | isJust(bdis s) && isJust(getManyD) = getManyD
       | otherwise = Nothing
     
     where getManyD = manyDis$fromJust$bdis s

manyCon :: (BExp,String) -> Maybe (BExp,String)
manyCon (e,[]) = Just(e,[])
manyCon (e,(s:ss)) | s == '|' && isJust getCon = manyCon(Or e (fst fromJCon),snd fromJCon)
                   | otherwise = Just(e,(s:ss))
              
    where getCon = bcon ss
          fromJCon = fromJust getCon

bdis :: String -> Maybe (BExp,String)
bdis [] = Nothing
bdis (s:ss) | isLower s = Just(Bvar s,ss)
            | s == '(' && isJust(bexpr) && head(snd fromJBexpr) == ')' =
               Just(fst fromJBexpr,tail$snd fromJBexpr)
            | s == '!' && isJust(getBdis) = Just(Not (fst fromJBdis),snd fromJBdis)
            | s == 'T' = Just(Bvalue True, ss)
            | s == 'F' = Just(Bvalue False, ss)
            | otherwise = Nothing
    
    where bexpr = bexp ss 
          fromJBexpr = fromJust bexpr
          getBdis = bdis ss
          fromJBdis = fromJust getBdis

manyDis :: (BExp,String) -> Maybe (BExp,String)
manyDis (e,[]) = Just(e,[])
manyDis (e,(s:ss)) | s == '&' && isJust getDis = manyDis(And e (fst fromJDis),snd fromJDis)
                   | otherwise = Just(e,(s:ss))

    where getDis = bdis ss
          fromJDis = fromJust getDis

------------------------------------------------------

bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 :: String
bs1 = "F"
bs2 = "!(x&(F|y))"
bs3 = "u&T"
bs4 = "d&(x|!y)"
bs5 = "!(d&(x|!y))"
bs6 = "u&x|y&z" 
bs7 = "!y|(x|!e)"
bs8 = "u|!u"
bs9 = "z&(y|!y&x)"

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Bvalue False
b2 = Not (And (Bvar 'x') (Or (Bvalue False) (Bvar 'y')))
b3 = And (Bvar 'u') (Bvalue True)
b4 = And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y')))
b5 = Not (And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y'))))
b6 = Or (And (Bvar 'u') (Bvar 'x')) (And (Bvar 'y') (Bvar 'z'))
b7 = Or (Not (Bvar 'y')) (Or (Bvar 'x') (Not (Bvar 'e')))
b8 = Or (Bvar 'u') (Not (Bvar 'u'))
b9 = And (Bvar 'z') (Or (Bvar 'y') (And (Not (Bvar 'y')) (Bvar 'x')))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(2,('x',4,5)),(4,('y',1,1)),(5,('y',1,0))])
bdd3 = (5,[(5,('u',0,1))])
bdd4 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('d',0,1)),(9,('d',0,0)),
           (5,('y',10,11)),(10,('d',0,1)),(11,('d',0,1))])
bdd5 = (3,[(4,('y',8,9)),(3,('x',4,5)),(8,('d',1,0)),(9,('d',1,1)),
           (5,('y',10,11)),(10,('d',1,0)),(11,('d',1,0))])
bdd6 = (2,[(2,('u',4,5)),(4,('x',8,9)),(8,('y',16,17)),(16,('z',0,0)),
           (17,('z',0,1)),(9,('y',18,19)),(18,('z',0,0)),(19,('z',0,1)),
           (5,('x',10,11)),(10,('y',20,21)),(20,('z',0,0)),(21,('z',0,1)),
           (11,('y',22,23)),(22,('z',1,1)),(23,('z',1,1))])
bdd7 = (6,[(6,('x',4,5)),(4,('y',8,9)),(8,('e',1,1)),(9,('e',1,0)),
           (5,('y',10,11)),(10,('e',1,1)),(11,('e',1,1))])
bdd8 = (2,[(2,('u',1,1))])
bdd9 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('z',0,0)),(9,('z',0,1)),(5,('y',10,11)),(10,('z',0,1)),(11,('z',0,1))])



