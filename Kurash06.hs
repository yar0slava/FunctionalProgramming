{-# OPTIONS_GHC -Wall #-}
module Kurash06 where

newtype Poly a = P [a]

-- Задача 1 -----------------------------------------
x :: Num a => Poly a
x = P[0,1]

-- Задача 2 ----------------------------------------
instance (Num a, Eq a) => Eq (Poly a) where
    P[] == P[] = True
    P[] == P(zs) | all (==0) zs = True
    P(z:zs) == P(y:ys) = z==y && zs==ys
    _ == _ = False
 
-- Задача 3 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P[]) = show (0::Integer)
    show (P[y]) = show y
    show (P ys) | all (==0) ys = show (0::Integer)
                | otherwise = head(reverse[showCE (ys!!n) n | n <- [0..(length ys-1)], ys!!n /= 0]) ++ 
                   concat (map (" + "++) (tail (reverse[showCE (ys!!n) n | n <- [0..(length ys-1)], ys!!n /= 0])))
      where showCE c e | e==0 = show c
                       | e==1 && c==1 = "x"
                       | e==1 && c==(-1) = '-':"x"

                       | c==(-1) = '-':('x':('^':show e))
                       | c==(1) = 'x':('^':show e)

                       | e==1 = (show c) ++ "x"
                       | otherwise = (show c) ++ 'x':('^':show e)

-- Задача 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P[]) (P[]) = P[]
plus p (P[]) = p
plus (P[]) p = p
plus (P(zs)) (P(ys)) | length zs > length ys = P([(zs!!n) + (ys!!n) | n <- [0..(length ys -1)]] ++ [zs!!m | m <-[(length ys)..(length zs-1)]])
                     | length zs < length ys = P([(zs!!n) + (ys!!n) | n <- [0..(length zs -1)]] ++ [ys!!m | m <-[(length zs)..(length ys-1)]])
                     | otherwise = P[(zs!!n) + (ys!!n) | n <- [0..(length zs -1)]]

-- Задача 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P[]) (P[]) = P[]
times p (P[]) = p
times (P[]) p = p
times (P(zs)) p = foldl1 plus [timesN n (zs!!n) p | n <- [0..(length zs -1)]]

timesN :: Num a => Int -> a -> Poly a -> Poly a
timesN n b (P(ys)) | n == 0 = P[ y*b | y <- ys] 
                   | otherwise = P((repeat 0)++[y*b | y <- ys])

-- Задача 6 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P[]) = P[]
    negate (P(zs)) = P[-z | z<-zs]
    fromInteger i = P [fromInteger i]
    
    abs    = undefined
    signum = undefined

-- Задача 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP (P[]) _ = 0
applyP (P(zs)) n = foldl1 (+) [(zs!!m)*(n^m) | m <- [0..(length zs -1)]]

-- Задача 8 -----------------------------------------
class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 1 y = deriv y
    nderiv n y = deriv(nderiv (n-1) y)

-- Задача 9 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    deriv (P[]) = P[]
    deriv (P(zs)) = P[(zs!!m)*(fromIntegral m) | m <- [1..(length zs -1)]]

