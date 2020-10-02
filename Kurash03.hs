{-# OPTIONS_GHC -Wall #-}
module Kurash03 where

type Code = String
data Move = Move Code Int Int
          deriving (Show, Eq)

bull :: Move -> Int
bull (Move _ f _) = f

cow :: Move -> Int
cow (Move _ _ p) = p

-- Завдання 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches (x:[]) (y:[]) = if x==y then 1 else 0
exactMatches (x:xs) (y:ys) = if x==y then 1+exactMatches xs ys
 else 0+exactMatches xs ys 
exactMatches [] _ = 0 
exactMatches _ [] = 0 


-- Завдання 2 -----------------------------------------
countDigits :: Code -> [Int]
countDigits cd = countDigitsAssist cd 0

countDigitsAssist :: Code -> Int -> [Int]
countDigitsAssist [] y = if y < 9 then 0:countDigitsAssist [] (y+1)
 else 0 : []
countDigitsAssist xs y = if y < 9 then (countNum xs y) : (countDigitsAssist xs (y+1))
 else (countNum xs 9) : []

countNum :: Code -> Int -> Int
countNum [] _ = 0
countNum xs y = if (head xs) == head(show y) then 1 + countNum (tail xs) y
 else  0 + countNum (tail xs) y

-- Завдання 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches cd att = foldr (+) 0 (map2 min (countDigits cd) (countDigits att))
 
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f xs ys = f (head xs) (head ys) : map2 f (tail xs) (tail ys) 
-- Завдання 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd att = (Move att (exactMatches cd att) (matches cd att - exactMatches cd att))

-- Завдання 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move att f p) cd = (exactMatches cd att)==f && (matches cd att - exactMatches cd att) == p

-- Завдання 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cdx = [x | x <- cdx, isConsistent mv x]

-- Завдання 7 -----------------------------------------
allCodes :: Int -> [Code]
allCodes n = extand [] n

extand :: [Code] -> Int -> [Code]
extand cdx d = if d == 1 then [show x|x <- [0..9::Integer]]
 else [head(show y):xs| y<-[0..9::Integer], xs<- extand cdx (d-1)]
   
-- Завдання 8 -----------------------------------------
solve :: Code -> [Move]
solve cd = solveAssist cd [] []

solveAssist :: Code -> [Code] -> [Move] -> [Move]
solveAssist cd [] [] = solveAssist cd (allCodes(lngth cd)) []
solveAssist cd atts mvs = if bull(getMove cd (head atts))==(lngth cd) then concatFr mvs [getMove cd (head atts)]
 else solveAssist cd (filterCodes (getMove cd (head atts)) atts) (concatFr mvs [getMove cd (head atts)])

concatFr :: [Move] -> [Move] -> [Move]
concatFr xs ys = if null xs then ys else
 (if null ys then xs else foldr (:) ys xs)

lngth :: String -> Int
lngth xs = if null xs then 0 
 else 1 + lngth(tail xs)