{-# OPTIONS_GHC -Wall #-}
module Kurash05 where
import Data.List

type Graph = [[Int]]

-- Задача 1 ------------------------------------------
lucky :: Int ->  [String]
--lucky = undefined
lucky n | n < 1 = error "n must be > 0"
        | otherwise = [toString x | x <- allCombinations (n+n), equalSums n x]

allCombinations :: Int ->  [Int]
allCombinations m = [x | x <- [10^(m-1)..(10^m)-1]]

toString :: Int -> String
toString 0 = []
toString x = (toString (div x 10)) ++ show(mod x 10)

equalSums :: Int -> Int -> Bool
equalSums n m | sumOfNums(div m (10^n)) == sumOfNums(mod m (10^n)) = True
              | otherwise = False

sumOfNums :: Int -> Int
sumOfNums 0 = 0
sumOfNums x = (sumOfNums (div x 10)) + mod x 10

-- Задача 2 -----------------------------------------  
queens ::  Int -> [[Int]]
queens n = [xs | xs <-allPlacements n, isCorrect xs]

allPlacements :: Int -> [[Int]]
allPlacements n = until (cond n) (step n) [[x] | x <- [1..n]]

cond :: Int -> [[Int]] -> Bool
cond n xss = length(head xss) == n

step :: Int -> [[Int]] -> [[Int]]
step n xss = [ y:xs | xs <- xss, y <- [1..n], notElem y xs]

isCorrect :: [Int] -> Bool
isCorrect xs = foldl1 (&&) [ abs(x-y)/=abs(xs!!x - xs!!y) | x <- [0..((length xs) - 2)], y <- [(x+1)..((length xs) - 1)]]
   
-- Задача 3 -----------------------------------------
maxLen ::  [Int] -> Int
maxLen [] = 0
maxLen ys = maximum [length xs | xs <- (allSeq (removeDublicates ys)) ] 

-- Задача 4 -----------------------------------------
maxSeq ::  [Int] ->  [Int]
maxSeq [] = []
maxSeq ys = head(allMaxSeq ys)

-- Задача 5 -----------------------------------------
allMaxSeq ::  [Int] -> [[Int]]
allMaxSeq [] = []
allMaxSeq  ys = [xs | xs <- (allSeq (removeDublicates ys)), (length xs) == (maxLen ys) ] 

removeDublicates :: [Int] -> [Int]
removeDublicates [] = []
removeDublicates [x] = [x]
removeDublicates (x:xs) | x==head xs = removeDublicates xs
                        | otherwise = x:(removeDublicates xs)

allSeq :: [Int] -> [[Int]]
allSeq [] = []
allSeq [x] = [[x]]
allSeq (x:(y:[])) | x < y = [[x], [y], [x,y]]
                  | otherwise = [[x], [y]]
allSeq (x:xs) = allSeq xs++[x:y | y <- allSeq xs, (head y) > x]

-- Задача 6 -----------------------------------------
genExpr ::  Int -> Int -> [String]
genExpr x y = [fst p | p<-buildExpr(reverse(toList x)), snd p == y]

buildExpr :: [Int] -> [(String, Int)]
buildExpr [] = [([],0)]
buildExpr [x] = [(show x, x)]
buildExpr (x:xs) = [( fst(y)++('*':show x), (snd y)*x ) | y<-buildExpr xs]++
                   [( fst(y)++('+':show x), (snd y)+x ) | y<-buildExpr xs]++
                   [( fst(y)++('-':show x), (snd y)-x ) | y<-buildExpr xs]

toList :: Int -> [Int]
toList 0 = []
toList x = toList (div x 10) ++ [mod x 10]

-- Задача 7 -----------------------------------------
genExprBracket ::  Int -> Int -> [String]
genExprBracket = undefined

-- Задача 8 -----------------------------------------
topolSortAll :: Graph -> [[Int]]
topolSortAll gr = topolSortAllAssist (nodes gr) (edges gr)

topolSortAllAssist :: [Int] -> [(Int,Int)] -> [[Int]]
topolSortAllAssist [] _ = []
topolSortAllAssist [n] [] = [[n]]
topolSortAllAssist nds [] = [ n:s | n <- nds, s <- (topolSortAllAssist (nds\\[n]) [])] 
topolSortAllAssist nds edgs = [m:s | m <- mins, s <- (topolSortAllAssist (nds\\[m]) (remainingEdges edgs m))]
 where mins = [x | x <- nds, isMin edgs x ]

remainingEdges :: [(Int,Int)] -> Int -> [(Int,Int)]
remainingEdges [] _ = []
remainingEdges edgs m = [y | y<-edgs, (fst y) /= m]

isMin :: [(Int,Int)] -> Int -> Bool
isMin [] _ = True
isMin edgs v = foldl1 (&&) [(snd y) /= v| y <- edgs]

edges :: Graph -> [(Int,Int)]
edges g = [(x,y)| x<-nodes g, y<- g!!x]

nodes:: Graph -> [Int]
nodes g = [0..(length g - 1)]

--------------------------------------------
gr1 :: Graph 
gr1 = [[1,2,3], [], [3,4], [4],[]]