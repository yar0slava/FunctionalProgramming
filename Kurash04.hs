{-# OPTIONS_GHC -Wall #-}
module Kurash04 where
import Data.List

type Graph  = [[Int]]


nodes:: Graph -> [Int]
nodes g = [0..(length g - 1)]

edges :: Graph -> [(Int,Int)]
edges g = [(x,y)| x<-nodes g, y<- g!!x]

-- Задача 1 ------------------------------------
isGraph :: Graph -> Bool 
isGraph gr = foldl1 (&&) [(isSet x)&&(containsOnly x (nodes gr)) | x <- gr]

containsOnly :: [Int] -> [Int] -> Bool
containsOnly [] _ = True
containsOnly _ [] = False
containsOnly xs ys = foldl1 (&&) [elem x ys | x<-xs]

-- Задача 2 ------------------------------------
isTournament :: Graph -> Bool 
isTournament gr | isGraph gr = containsAllSimilar (edges gr) (allEdges gr)
                | otherwise = error "graph must be oriented"

allEdges :: Graph -> [(Int,Int)]
allEdges gr = [(x,y)| x<-[0..(length gr - 2)], y <- [x+1..(length gr - 1)]]

containsAllSimilar :: [(Int,Int)] -> [(Int,Int)] -> Bool
containsAllSimilar _ [] = True
containsAllSimilar ys (s:xs) | containsSimilar ys s = containsAllSimilar ys xs
                             | otherwise = False 

containsSimilar :: [(Int,Int)] -> (Int,Int) -> Bool
containsSimilar [] _ = False
containsSimilar (s:xs) y | s==y || ((fst s)==(snd y) && (fst y)==(snd s)) = True
                         | otherwise = containsSimilar xs y

-- Задача 3 ------------------------------------
isTransitive :: Graph -> Bool 
isTransitive gr = foldl1 (&&) [containsAll (edges gr) (findAllTransitive x (edges gr))|x<-edges gr] 

findAllTransitive :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
findAllTransitive _ [] = []
findAllTransitive x xs = [((fst x),j)|(i,j)<-xs, i==(snd x)]

containsAll :: [(Int,Int)] -> [(Int,Int)] -> Bool
containsAll _ [] = True
containsAll ys (s:xs) | contains ys s = containsAll ys xs
                      | otherwise = False 

contains :: [(Int,Int)] -> (Int,Int) -> Bool
contains [] _ = False
contains (s:xs) y | s==y || ((fst s)==(snd y) && (fst y)==(snd s)) = True
                  | otherwise = contains xs y

-- Задача 4 ------------------------------------
buildTransitive :: Graph -> Graph 
buildTransitive gr = edgesToGraph gr (snd (until condition4 step4 (init4(edges gr))))

condition4 :: ([(Int,Int)],[(Int,Int)]) -> Bool
condition4 (new, _) = null new

step4 :: ([(Int,Int)],[(Int,Int)]) -> ([(Int,Int)],[(Int,Int)])
step4 (ns, os) = (difference new old, old)
 where old = ns++os
       new = difference (set(foldl1 (++) [findAllTransitive x old|x<-old])) old 

init4 :: [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
init4 [] = ([], [])
init4 os = (difference (set(foldl1 (++) [findAllTransitive x os|x<-os])) os,os)

set :: [(Int,Int)] -> [(Int,Int)]
set [] = []
set (s:xs) = s:set(filter(/=s)xs)

difference :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
difference xs ys = set (xs\\ys)

edgesToGraph :: Graph -> [(Int,Int)] -> Graph 
edgesToGraph _ [] = []
edgesToGraph gr edgs = [sort [snd x|x<-edgs, (fst x) == z]| z <- [0..(length gr - 1)]]

-- Задача 5 ------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int] 
longWay gr v u | isGraph gr = if (longWayAssist gr v u) == [] then Nothing
 else Just (longWayAssist gr v u)
               | otherwise = error "graph must be oriented"

longWayAssist :: Graph -> Int -> Int -> [Int] 
longWayAssist gr v u = reverse(theLongest(toListWithEnd u (allSimpleWays gr v))) 

allSimpleWays :: Graph -> Int -> [[[Int]]] 
allSimpleWays gr v = until cond5 (step5 gr) [[[v]]]

cond5 :: [[[Int]]] -> Bool 
cond5 wss = null(head wss)

step5 :: Graph -> [[[Int]]] -> [[[Int]]] 
step5 gr wss@(wsn:_) = [t:w | w@(x:xs) <- wsn, t<-gr!!x, notElem t xs]:wss
step5 _ [] = error "allWays:stepW"

toListWithEnd :: Int -> [[[Int]]] -> [[Int]]
toListWithEnd u wws = [x | ws<-wws, x<-ws, head x == u]

theLongest :: [[Int]] -> [Int]
theLongest [] = []
theLongest (x:[]) = x
theLongest (x:[y]) | length x > length y = x
                   | otherwise = y
theLongest (x:xs) | length x > length (head xs) = theLongest (x:(tail xs))
                  | otherwise = theLongest xs

-- Задача 6 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr | isGraph gr = if (length gr)+1 == length(gamiltonWayAssist gr) then Just (reverse(gamiltonWayAssist gr))
 else Nothing
               | otherwise = error "graph must be oriented"

gamiltonWayAssist :: Graph -> [Int]
gamiltonWayAssist gr = theLongest(toListWithEnd 0 (allSimpleCycles gr 0))

allSimpleCycles :: Graph -> Int -> [[[Int]]] 
allSimpleCycles gr v = until cond5 (step6 gr) [[[v]]]

step6 :: Graph -> [[[Int]]] -> [[[Int]]] 
step6 gr wss@(wsn:_) = [t:w | w@(x:xs) <- wsn, t<-gr!!x, notElem t (xs\\[0])]:wss
step6 _ [] = error "allWays:stepW"

-- Задача 7 ------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic gr = foldl1 (&&) [snd(allWays gr x)|x <- [0..(length gr - 1)]]

allWays :: Graph -> Int -> ([[[Int]]], Bool)
allWays gr v = until cond7 (step7 gr) ([[[v]]], True)

cond7 :: ([[[Int]]], Bool) -> Bool 
cond7 (wss, b) = null(head wss) || not b

step7 :: Graph -> ([[[Int]]], Bool) -> ([[[Int]]], Bool)
step7 gr (wss@(wsn:_), b) = ([t:w | w@(x:xs) <- wsn, t<-gr!!x, notElem x xs]:wss, foldl (&&) b [isSet y| y<-wsn])
step7 _ ([], _) = error "allWays:stepW"

isSet :: [Int] -> Bool
isSet [] = True
isSet (s:xs) | elem s xs = False
             | otherwise = isSet xs

-- Задача 8 ------------------------------------
topolSort :: Graph -> Maybe [Int] 
topolSort gr | isGraph gr = if not (isAcyclic gr) then Nothing
 else Just (topolSortAssist gr (edges gr) [])
             | otherwise = error "graph must be oriented"

topolSortAssist :: Graph -> [(Int,Int)] -> [Int] -> [Int]
topolSortAssist gr [] nds = nds++([0..(length gr - 1)]\\nds)
topolSortAssist gr edgs nds = topolSortAssist gr newEdgs (nds++mins)
 where mins = [x | x <- ([0..(length gr - 1)]\\nds), isMin edgs x ]
       newEdgs = [y | y<-edgs, notElem (fst y) mins]

isMin :: [(Int,Int)] -> Int -> Bool
isMin [] _ = True
isMin edgs v = foldl1 (&&) [(snd y) /= v| y <- edgs]

-- Задача 9------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort gr nods | isGraph gr = isTopolSortAssist (edges gr) nods 
                    | otherwise = error "graph must be oriented"

isTopolSortAssist :: [(Int,Int)] -> [Int] -> Bool 
isTopolSortAssist [] [] = True
isTopolSortAssist (_:_) [] = False
isTopolSortAssist edgs (x:xs) | not(isMin edgs x) = False
                              | otherwise = 
 isTopolSortAssist [y | y<-edgs, (fst y) /= x] xs


gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]