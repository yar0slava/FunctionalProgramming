{-# OPTIONS_GHC -Wall #-}
module Kurash01 where

-- Завдання 1 -----------------------------------------
factorial :: Integer -> Integer
factorial n = if n <=1 then 1 else n * factorial (n-1)

-- Завдання 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum xs ys = if null xs && null ys then [] else 
 (if null xs then head ys : listSum [] (tail ys) else 
  (if null ys then head xs : listSum [] (tail xs) else 
   (head xs + head ys) : listSum (tail xs) (tail ys)))
 

-- Завдання 3 ----------------------------------------- 
oddEven :: [Int] -> [Int] 
oddEven xs = if null xs then [] else 
 if null (tail xs) then xs else 
  (head(tail xs) : (head xs : oddEven (tail (tail xs))))

-- Завдання 4 -----------------------------------------
position    ::  Int -> [Int] -> Int
position n xs = positionAsist n 0 xs

positionAsist :: Int -> Int -> [Int] -> Int
positionAsist n m xs = if null xs then -1 else 
 (if head xs == n then m else positionAsist n (m+1) (tail xs))
                     
-- Завдання 5 -----------------------------------------
set :: [Int] -> [Int] 
set xs = if null xs then [] else head xs : set (remove (head xs) (tail xs))

remove :: Int -> [Int] -> [Int] 
remove x xs = [y| y <- xs, y /= x]

-- Завдання 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union xs ys = set(concatMy xs ys) 

concatMy :: [Int] -> [Int] -> [Int]
concatMy xs ys = if null xs then ys else
 (if null ys then xs else (head xs) : concatMy (tail xs) ys)

-- Завдання 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = set [x | x <- xs,  y <- ys, x == y]

-- Завдання 8 -----------------------------------------
factorialsM :: [Integer]
factorialsM = [factorial x | x <- [1,2..]]