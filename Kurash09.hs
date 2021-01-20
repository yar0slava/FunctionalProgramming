{-# OPTIONS_GHC -Wall #-}
module Kurash09 where

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Ord, Show)

-- Задача 1 -----------------------------------------
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) | x==y = isPrefix xs ys
                       | otherwise = False

-- Задача 2 -----------------------------------------
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition [] s = ([],[],s)
partition s [] = ([],s,[])
partition (x:xs) (y:ys) | x==y = (x:(first nextPart), second nextPart, third nextPart)
                        | otherwise = ([],(x:xs),(y:ys))
   where nextPart = partition xs ys

first :: Eq a => ([a], [a], [a])-> [a] 
first (s,_,_) = s

second :: Eq a => ([a], [a], [a])-> [a] 
second (_,s,_) = s

third :: Eq a => ([a], [a], [a])-> [a] 
third (_,_,s) = s

-- Задача 3 -----------------------------------------
suffixes :: [a] -> [[a]]
suffixes [] = error "enter not empty string"
suffixes xs = snd$until suffixesCond suffixesStep (xs,[])

suffixesCond :: ([a], [[a]]) -> Bool
suffixesCond (xs,_) = null xs

suffixesStep :: ([a], [[a]]) -> ([a], [[a]])
suffixesStep (xs,ys) = (tail xs, ys++[xs])

-- Задача 4 -----------------------------------------
isSubstring :: String -> String -> Bool
isSubstring s t = foldl1 (||) [isPrefix s x | x <- suffixes t]

-- Задача 5 -----------------------------------------
findSubstrings :: String -> String -> [Int]
findSubstrings _ [] = error "enter not empty string"
findSubstrings s t = [n | n <- [0..length allSuff-1], isPrefix s (allSuff!!n)]
  where allSuff = suffixes t

-- Задача 6 -----------------------------------------
getIndices :: SuffixTree -> [Int]
getIndices (Leaf x) = [x]
getIndices (Node xs) = quicksort$concat [getIndices$snd x | x <- xs]

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

-- Задача 7 -----------------------------------------
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' [] tree = getIndices tree
findSubstrings' _ (Leaf _) = []
findSubstrings' s (Node xs) | not(null$findSubTreeContains s (Node xs)) = getIndices$head$findSubTreeContains s (Node xs)
                            | null nextTree = []
                            | otherwise = findSubstrings' (tail s) (head nextTree)
  where nextTree = findSubTreeStarts s (Node xs)

-- finds subtree with string that contains s
findSubTreeContains :: String -> SuffixTree -> [SuffixTree]
findSubTreeContains _ (Leaf _) = []
findSubTreeContains s (Node xs) = [snd x | x <- xs, isPrefix s (fst x)]

-- finds subtree with string that starts with s
findSubTreeStarts :: String -> SuffixTree -> [SuffixTree]
findSubTreeStarts _ (Leaf _) = []
findSubTreeStarts s (Node xs) = [snd x | x <- xs, isPrefix (fst x) s]

-- Задача 8 -----------------------------------------
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert = undefined

buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

-- Задача 9 -----------------------------------------
longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring = undefined

------------------------------------------------------

s1 :: String
s1 = "banana"

s2 :: String
s2  = "mississippi"

t1 :: SuffixTree
t1 = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]