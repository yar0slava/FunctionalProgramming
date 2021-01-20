{-# OPTIONS_GHC -Wall #-}
module Kurash10 where

data BinTree a = EmptyB 
                | Node a (BinTree a) (BinTree a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a   
               | Node2 (Tree23 a) a (Tree23 a) 
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23 
                   deriving (Eq, Show)

-- Задача 1 -----------------------------------------			   
isSearch :: (Ord a) => BinTree a -> Bool
isSearch (EmptyB) = True
isSearch (Node _ EmptyB EmptyB) = True
isSearch (Node v tl EmptyB) = maxVBinary tl < v && isSearch tl
isSearch (Node v EmptyB tr) = minVBinary tr > v && isSearch tr
isSearch (Node v tl tr) = minVBinary tr > v && isSearch tr && maxVBinary tl < v && isSearch tl

maxVBinary :: (Ord a) => BinTree a -> a
maxVBinary (Node x EmptyB EmptyB) = x
maxVBinary t = maximum$toListBinary t

minVBinary :: (Ord a) => BinTree a -> a
minVBinary (Node x EmptyB EmptyB) = x
minVBinary t = minimum$toListBinary t

isTerminalBinary :: (Ord a) => BinTree a -> Bool
isTerminalBinary (Node _ EmptyB EmptyB) = True
isTerminalBinary _ = False

-- Задача 2-----------------------------------------
elemSearch :: (Ord a) => BinTree a -> a -> Bool
elemSearch EmptyB _ = False
elemSearch t@(Node v tl tr) x | not$isSearch t = error "tree must be searching"
                              | x>v = elemSearch tr x
                              | x<v = elemSearch tl x
                              | otherwise = True

-- Задача 3 -----------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a 
insSearch EmptyB x = Node x EmptyB EmptyB
insSearch t@(Node v tl tr) x | not$isSearch t = error "tree must be searching"
                             | x>v = (Node v tl (insSearch tr x))
                             | x<v = (Node v (insSearch tl x) tr)
                             | otherwise = error "can not insert existing value"

-- Задача 4 -----------------------------------------
delSearch :: (Ord a) => BinTree a -> a -> BinTree a 
delSearch EmptyB _ = error "can not delete from empty tree"

delSearch (Node y EmptyB EmptyB) x | x==y = EmptyB
                                   | otherwise = error "can not delete non existing value"

delSearch t@(Node y tl EmptyB) x | not$isSearch t = error "tree must be searching"
                                 | x==y = tl
                                 | otherwise = (Node y (delSearch tl x) EmptyB)

delSearch t@(Node y EmptyB tr) x | not$isSearch t = error "tree must be searching"
                                 | x==y = tr
                                 | otherwise = (Node y EmptyB (delSearch tr x))

delSearch t@(Node v tl tr) x | not$isSearch t = error "tree must be searching"
                             | x>v = (Node v tl (delSearch tr x))
                             | x<v = (Node v (delSearch tl x) tr)
                             | otherwise = (Node mx tl1 tr)
          where mx = maxVBinary tl
                tl1 = delSearch tl mx
 
-- Задача 5 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList xs = toListBinary$foldl insSearch EmptyB xs

toListBinary :: (Ord a) => BinTree a -> [a]
toListBinary EmptyB = []
toListBinary (Node x EmptyB EmptyB) = [x]
toListBinary (Node v tl tr) = (toListBinary tl)++[v]++(toListBinary tr)

-- Задача 6-----------------------------------------
isTree23 :: (Ord a) => Tree23 a -> Bool 
isTree23 Empty23 = True
isTree23 (Leaf _)= True
isTree23 (Node2 (Leaf v1) x (Leaf v2)) = x==v2 && v1<=v2
isTree23 (Node2 tl x tr) = x==minV23 tr && isTree23 tl && isTree23 tr && (depth tl) == (depth tr)
isTree23 (Node3 (Leaf v1) x1 (Leaf v2) x2 (Leaf v3)) = v1<=v2 && v2<=v3 && x1==v2 && x2==v3
isTree23 (Node3 t1 x1 t2 x2 t3) = x1==minV23 t2 && x2==minV23 t3 && 
                                  (depth t1) == (depth t2) && (depth t2) == (depth t3)

toList23 :: (Ord a) => Tree23 a -> [a]
toList23 Empty23 = []
toList23 (Leaf x) = [x]
toList23 (Node2 tl _ tr) = (toList23 tl)++(toList23 tr)
toList23 (Node3 t1 _ t2 _ t3) = (toList23 t1)++(toList23 t2)++(toList23 t3)

minV23 :: (Ord a) => Tree23 a -> a
minV23 (Leaf x) = x
minV23 t = minimum$toList23 t

maxV23 :: (Ord a) => Tree23 a -> a
maxV23 (Leaf x) = x
maxV23 t = maximum$toList23 t

depth :: (Ord a) => Tree23 a -> Int
depth Empty23 = -1
depth (Leaf _) = 0
depth (Node2 tl _ tr) = 1+(max (depth tl) (depth tr))
depth (Node3 t1 _ t2 _ t3) = 1+(max (depth t1) (max (depth t2) (depth t3)))

-- Задача 7-----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 Empty23 _ = False
elemTree23 (Leaf x) y = x==y
elemTree23 t@(Node2 tl x tr) y | not$isTree23 t = error "tree must be balanced"
                               | y==x = True
                               | y>x = elemTree23 tr y
                               | otherwise = elemTree23 tl y

elemTree23 t@(Node3 t1 x1 t2 x2 t3) y | not$isTree23 t = error "tree must be balanced"
                                      | y==x1 || y==x2 = True
                                      | y<x1 = elemTree23 t1 y
                                      | y>x2 = elemTree23 t3 y
                                      | otherwise = elemTree23 t2 y

-- Задача 8-----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 t1 t2 = (toList23 t1) == (toList23 t2)

-- Задача 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23  = undefined

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True 
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- Результат вставки вузла в 2-3-дерево, 
--   корінь якого - вузол вида Node2 або Node3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a 
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)

--  insert v tr - додає значення v в довільне дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr

-- insTerm v tr - додається значення v в дерево tr з конем - термінальний вузол 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insNode v tr - додає значення v в дерево tr з корнем - нетермінальний вузол
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode = undefined



bt1, bt2 ::  BinTree Int
bt1 = Node 9 (Node 4 EmptyB (Node 8 EmptyB EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                       EmptyB)
bt2 = Node 9 (Node 4 EmptyB 
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                       EmptyB)

--tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 2 (Node2 (Leaf 2) 3 (Leaf 3))) 4 (Node2 (Node2 (Leaf 4) 5 (Leaf 5)) 6 (Node2 (Leaf 6) 7 (Leaf 7)))


tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8)))
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19)))