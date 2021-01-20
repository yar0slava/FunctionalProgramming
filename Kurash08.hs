{-# OPTIONS_GHC -Wall #-}
module Kurash08 where

import Data.Tree

-- Задача 1 -----------------------------------------			   
rank :: Tree a -> Int
rank (Node _ []) = 0
rank (Node _ xs) = length xs

-- Задача 2-----------------------------------------
isBinomTree :: Ord a => Tree a -> Bool
isBinomTree (Node _ []) = True
isBinomTree (Node x xs) = foldl1 (&&) [(null children || x<=minimum children) && isBinomTree(xs!!n) && 
                                       rank(xs!!n)+1==(length xs)-n | n <- [0..(length xs)-1]]
     where children = getChildren xs

getChildren :: Ord a => Forest a -> [a]
getChildren xs = [rootLabel x | x <- xs]


-- Задача 3 -----------------------------------------
isBinomHeap :: Ord a => Forest a -> Bool
isBinomHeap [] = error "forest must not be empty"
isBinomHeap [x] = isBinomTree x
isBinomHeap xs = foldl1 (&&) [isBinomTree(xs!!n) && rank(xs!!n) < rank(xs!!(n+1))
                       | n <- [0..(length xs)-2]] && isBinomTree(xs!!((length xs)-1))


-- Задача 4 -----------------------------------------
combineTrees :: Ord a => Tree a -> Tree a -> Tree a
combineTrees (Node x xs) (Node y ys) | x < y = (Node x ((Node y ys):xs))
                                     | otherwise = (Node y ((Node x xs):ys))

-- Задача 5 -----------------------------------------
extractMin :: Ord a => Forest a -> a
extractMin [] = error "forest must not be empty"
extractMin xs = minimum [rootLabel(x) | x <- xs]

-- Задача 6-----------------------------------------
mergeHeaps :: Ord a => Forest a -> Forest a -> Forest a
mergeHeaps [] xs = xs
mergeHeaps xs [] = xs
mergeHeaps (x:xs) (y:ys) | rank x < rank y = x:(mergeHeaps xs (y:ys))
                         | rank x > rank y = y:(mergeHeaps ys (x:xs))
                         | otherwise = mergeHeaps [combineTrees x y] (mergeHeaps xs ys)

-- Задача 7-----------------------------------------
insert :: Ord a => a -> Forest a -> Forest a
insert _ [] = error "forest must not be empty"
insert x xs = mergeHeaps [Node x []] xs

-- Задача 8-----------------------------------------
deleteMin :: Ord a => Forest a -> Forest a
deleteMin [] = error "forest must not be empty"
deleteMin [x] = reverse(subForest x) 
deleteMin xs = mergeHeaps ((reverse$fst findMin)++(tail$snd findMin)) (reverse$subForest$head$snd findMin)
      where findMin = until (cond8$extractMin xs) step8 ([], xs)

cond8 :: Ord a => a -> (Forest a, Forest a) -> Bool
cond8 a (_, ys) = (rootLabel$head ys) == a

step8 :: Ord a => (Forest a, Forest a) -> (Forest a, Forest a)
step8 (xs, ys) = ((head ys):xs, tail ys)


-- Задача 9-----------------------------------------
binomSort :: Ord a => [a] -> [a]
binomSort [] = []
binomSort xs = reverse$snd$until deleteAllcond deleteAllstep (addAll xs, [])

deleteAllcond :: Ord a => (Forest a, [a]) -> Bool
deleteAllcond ([],_) = True
deleteAllcond (_,_) = False

deleteAllstep :: Ord a => (Forest a, [a]) -> (Forest a, [a]) 
deleteAllstep (xs,ys) = (deleteMin xs, (extractMin xs):ys)

addAll :: Ord a => [a] -> Forest a
addAll xs = snd(until addAllcond addAllstep (xs, []))

addAllcond :: Ord a => ([a], Forest a) -> Bool
addAllcond (xs,_) = null xs

addAllstep :: Ord a => ([a], Forest a) -> ([a], Forest a)
addAllstep (xs,[]) = (tail xs, [Node (head xs) []])
addAllstep (xs,ys) = (tail xs, insert (head xs) ys)

-- Задача 10 -----------------------------------------
toBinary :: Forest a -> [Int]
toBinary [] = error "forest must not be empty"
toBinary f = toBinaryAssist 0 (f,[])

toBinaryAssist :: Int -> (Forest a, [Int]) -> [Int]
toBinaryAssist _ ([],ys) = ys
toBinaryAssist n (xs,ys) | length(subForest$head xs)==n = toBinaryAssist (n+1) (tail xs, 1:ys) 
                         | otherwise = toBinaryAssist (n+1) (xs, 0:ys)

-----------------------------------------------------  

t1, t2, t3, t4, t5, t6, t7, t8, t4and7 :: Tree Int


t1 = Node 4  []
t2 = Node 1 [Node 5 []]
t3 = Node 2 [Node 8 [Node 9 []], 
             Node 7 []]
t4 = Node 2 [Node 3 [Node 6 [Node 8 []], 
                     Node 10 []],
             Node 8 [Node 9 []],
             Node 7 []]


t5 = Node 4 [Node 6 [Node 8 []], 
                     Node 10 []]
t6 = Node 2 [Node 8 [Node 9 []], Node 7 []]
t7 = Node 2 [Node 4 [Node 6 [Node 8 []], Node 10 []],
             Node 8 [Node 9 []], 
             Node 7 []]


t8 = Node 12 [Node 16 []]

t4and7 = Node 2 [ Node 2 [Node 3 [Node 6 [Node 8 []], 
                          Node 10 []],
                          Node 8 [Node 9 []],
                          Node 7 []],
                  Node 4 [Node 6 [Node 8 []], Node 10 []],
                  Node 8 [Node 9 []], 
                  Node 7 []]



h1, h2, h3, h4, h5, h6, h7, h5and6, h1and4, h4andt2 :: Forest Int

h1 = [t2, t7]
h2 = [Node 1 [Node 12 [Node 16 []],
              Node 5 []],
      Node 2 [Node 4 [Node 6 [Node 8 []],
                      Node 10 []],
              Node 8 [Node 9 []],
              Node 7 []]]


h3 = [t1, t2, t4]



h4 = [t2, t5]
h5 = [t1, t8]


h6 = [Node 4 [],
      Node 1 [Node 4 [Node 6  [Node 8 []],
                      Node 10 []],
              Node 12 [Node 16 []],
              Node 5 []]]


h7 = [Node 4 [Node 4 [Node 12 [Node 16 []],
                      Node 5 []],
              Node 6 [Node 8 []],
              Node 10 []]]

h5and6 = [Node 4 [Node 12 [Node 16 []],
               Node 4 []],
      Node 1 [Node 4 [Node 6  [Node 8 []],
                      Node 10 []],
              Node 12 [Node 16 []],
              Node 5 []]]

h1and4 = [Node 1 [Node 2 [Node 4 [Node 6 [Node 8 []],
                                 Node 10 []],
                         Node 8 [Node 9 []],
                         Node 7 []],
                 Node 4 [Node 6  [Node 8 []],
                         Node 10 []],
                 Node 1 [Node 5 []],
                 Node 5 []]]

h4andt2 = [Node 1 [Node 4 [Node 6 [Node 8 []],
                          Node 10 []],
                   Node 1 [Node 5 []],
                   Node 5 []]]