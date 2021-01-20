{-# OPTIONS_GHC -Wall #-}
module Kurash00 where

data Quaternion = Quaternion Double Double Double Double 
                  deriving (Eq)

type Graph  = [[Int]]
data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                  deriving (Show, Eq) 

-- Задача 1 -----------------------------------------
group :: Eq a => [a] -> [[a]]
group xs = reverse$fst$getAll ([],xs)

-- (to, from)
getAll :: Eq a => ([[a]],[a]) -> ([[a]],[a])
getAll (xss,[]) = (xss,[]) 
getAll ([],ys) = getAll ([[head ys]], tail ys)
getAll (xss, ys) | head(head xss) == head ys = getAll (((head ys):(head xss)):(tail xss), tail ys)
                 | otherwise = getAll ([head ys]:xss, tail ys)

-- (to, from)
getOne :: Eq a => ([a],[a]) -> ([a],[a])
getOne (xs,[]) = (xs,[])
getOne ([],ys) = getOne ([head ys], tail ys)
getOne (xs,ys) | head xs == head ys = getOne ((head ys):xs,tail ys)
               | otherwise = (xs,ys)
   
-- Задача 2 -----------------------------------------
bagSubbag :: String -> String -> Bool
bagSubbag a b = compareSubbag (quicksort a, quicksort b)

-- (subSet,set)
compareSubbag :: ([Char],[Char]) -> Bool
compareSubbag ([],_) = True
compareSubbag (_,[]) = False
compareSubbag (xs,ys) | head xs == head ys = compareSubbag (tail xs, tail ys)
                      | head xs > head ys = compareSubbag (xs, tail ys)
                      | otherwise = False

-- https://github.com/ArthurDetant/Prog_Fonctionnelle/blob/
-- fe3f01db282194f023fadca414b8db3858ff2149/Quicksort.hs

quicksort :: [Char] -> [Char]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

-- Задача 3 -----------------------------------------
bagUnion :: String -> String -> String
bagUnion a b = union (quicksort a, quicksort b)

-- (subSet,set)
union :: ([Char],[Char]) -> [Char]
union ([],xs) = xs
union (xs,[]) = xs
union (xs,ys) | head xs == head ys = (head xs):(union (tail xs, tail ys))
              | head xs > head ys = (head ys):(union (xs, tail ys))
              | otherwise = (head xs):(union (tail xs, ys))

-- Задача 4 -----------------------------------------
trianglePas :: [[Integer]]
trianglePas = [1]:(next [1])
    where next x = nextRow x : next (nextRow x)

nextRow :: [Integer] -> [Integer]
nextRow xs = 1:[xs!!n + xs!!(n+1) | n <- [0..(length xs - 2)] ]++[1]

-- Задача 5 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]
frequency [] = []
frequency xs = [(y, count y xs)| y<- set xs]

-- counts elem 
count :: Int -> [Int] -> Int
count _ [] = 0
count x xs | head xs == x = 1+count x (tail xs)
           | otherwise = count x (tail xs)

-- set from Lab 1
set :: [Int] -> [Int] 
set xs = if null xs then [] else head xs : set (remove (head xs) (tail xs))

remove :: Int -> [Int] -> [Int] 
remove x xs = [y | y <- xs, y /= x]
   
-- Задача  6 -----------------------------------------
instance Show Quaternion where
   show (Quaternion h i j k) = 
    show h ++
        (if i >= 0 then "+" ++ show i ++ "i" else show i ++ "i") ++ 
        (if j >= 0 then "+" ++ show j ++ "j" else show j ++ "j") ++
        (if k >= 0 then "+" ++ show k ++ "k" else show k ++ "k") 

-- Задача 7 -----------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion (Quaternion h1 i1 j1 k1) (Quaternion h2 i2 j2 k2) =
  Quaternion (h1+h2) (i1+i2) (j1+j2) (k1+k2)

-- Задача 8 -----------------------------------------
timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion (Quaternion h1 i1 j1 k1) (Quaternion h2 i2 j2 k2) =
  Quaternion (h1*h2 - i1*i2 - j1*j2 - k1*k2)
             (h1*i2 + h2*i1 + j1*k2 - j2*k1)
             (h1*j2 + h2*j1 - i1*k2 + k1*i2)
             (h1*k2 + h2*k1 + i1*j2 - i2*j1)

--- Задача 9 ----------------------------------------
instance Num Quaternion  where
    (+)   = plusQuaternion
    (*)   = timesQuaternion
    negate (Quaternion h i j k) = (Quaternion (negate h) (negate i) (negate j) (negate k))
    fromInteger x               = (Quaternion (fromInteger x) 0 0 0)
    abs q                       = (Quaternion (sqrt' q) 0 0 0)
    signum q@(Quaternion h i j k) = (Quaternion (h/sq) (i/sq) (j/sq) (k/sq)) 
       where sq = sqrt' q

sqrt' :: Quaternion -> Double
sqrt' (Quaternion h i j k) = sqrt(h*h + i*i + j*j + k*k)

-- Задача 10 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int]
shortWay gr a b | null (asssist10 b (head (until (cond10 b) (step10 gr) [[[a]]]))) = []
                | otherwise = reverse$head$asssist10 b (head (until (cond10 b) (step10 gr) [[[a]]]))

cond10 :: Int -> [[[Int]]] -> Bool
cond10 i xs = null(head xs) || not(null(asssist10 i (head xs)))

asssist10 :: Int -> [[Int]] -> [[Int]]
asssist10 b wss = [ws | ws <- wss, elem b ws]

step10 :: Graph -> [[[Int]]] -> [[[Int]]]
step10 _ [] = error "graph can not be empty"
step10 gr wss@(wsn:_)  = [t:w | w@(x:xs) <- wsn, notElem x xs, t<- gr!!x] : wss

-- Задача 11 ------------------------------------
isConnecting :: Graph -> Bool
isConnecting gr = foldl1 (&&) [not (null (shortWay gr 0 x)) | x<-[1..(length gr - 1)]]

-- Задача 12 ------------------------------------
components :: Graph -> [[Int]] 
components g = set12 [getComp g x | x <- [0..(length g - 1)]]

getComp :: Graph -> Int -> [Int]
getComp g v = [x | x <- [0..(length g - 1)], not(null$shortWay g v x)]

set12 :: [[Int]] -> [[Int]]
set12 xs = if null xs then [] else head xs : set12 (remove12 (head xs) (tail xs))

remove12 :: [Int] -> [[Int]] -> [[Int]] 
remove12 x xs = [y | y <- xs, y /= x]

-- Задача 13 ------------------------------------
eccentricity :: Graph -> Int -> Int
eccentricity g v= maximum $ map (\x -> (assist13 g v x)) [0..(length g-1)]

assist13 :: Graph -> Int -> Int -> Int
assist13 g v x = (length $ shortWay g v x) - 1

-- Задача 14 ------------------------------------
findDiameter :: Graph -> Int 
findDiameter [] = 0
findDiameter g = maximum [eccentricity g n | n <- [0..(length g - 1)]]

findRadius :: Graph -> Int 
findRadius [] = 0
findRadius g =  minimum [eccentricity g n | n <- [0..(length g - 1)]]

-- Задача 15 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter g = case isConnecting g of True -> [v | v <- nodes g, eccentricity g v == findRadius g]
                                      _    -> []

nodes::Graph->[Int]
nodes g=[0..((length g)-1)]
   
-- Задача 16 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM a n l r) | n<0 || n==0 = False
                         | l==EmptyM && r==EmptyM = True
                         | l==EmptyM = (getVal r)>a && isSearch r && lt a rt
                         | r==EmptyM = (getVal l)<a && isSearch l && gt a lft
                         | otherwise = and[(getVal r)>a,(getVal l)<a, isSearch r , isSearch l, lt a rt, gt a lft]
    where lft = elemsList l
          rt = elemsList r
          getVal (NodeM v _ _ _) = v
          getVal EmptyM = error "error"


elemsList :: (Ord a) => BinTreeM a -> [a]
elemsList EmptyM  = []
elemsList (NodeM a _ l r) | l==EmptyM && r==EmptyM = if (r==EmptyM) then [a] else [a] ++ elemsList r 
                          | r==EmptyM = elemsList l ++ [a]
                          | otherwise = elemsList l ++ [a] ++ elemsList r

gt :: (Ord a) => a -> [a] -> Bool
gt _ [] = True
gt a (x:xs) =  if (a<=x )then False else (gt a xs)

lt :: (Ord a) => a -> [a] -> Bool
lt _ [] = True
lt a (x:xs) = if (a>=x) then False else (lt a xs)

-- Задача 17 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM a _ l r) v | v > a = elemSearch r v
                             | v < a = elemSearch l v
                             | otherwise = True

-- Задача 18 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM v = (NodeM v 1 EmptyM EmptyM)
insSearch (NodeM a n l r ) v | v < a = NodeM a n (insSearch l v) r
                             | v > a = NodeM a n l (insSearch r v)
                             | otherwise = NodeM a (n+1) l r 

-- Задача 19 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch EmptyM _ = EmptyM
delSearch (NodeM a n l r) v | a==v && n>1 = NodeM a (n-1) l r
                            | a==v && n==1 = delRoot (NodeM a n l r)
                            | v < a = NodeM a n (delSearch l v) r
                            | otherwise = NodeM a n l (delSearch r v)

delRoot :: (Ord a) => BinTreeM a -> BinTreeM a
delRoot EmptyM = error "Can not delete root from empty tree"
delRoot (NodeM _ _ l r) | l==EmptyM = r
                        | r==EmptyM = l
                        | otherwise = NodeM (firstL r) (elNum r (firstL r)) l (delSearch r (firstL r))

firstL :: (Ord a) => BinTreeM a -> a
firstL EmptyM = error "tree can not be empty"
firstL (NodeM v _ EmptyM _) = v
firstL (NodeM _ _ l _) = firstL l

elNum :: (Ord a) => BinTreeM a -> a -> Int
elNum EmptyM _ = error "tree can not be empty"
elNum (NodeM a n l r) v | v>a = elNum r v
                        | v<a = elNum l v
                        | otherwise = n

-- Задача 20 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList xs = toList$foldl (insSearch) EmptyM xs

toList :: (Ord a) => BinTreeM a -> [a]
toList EmptyM = []
toList (NodeM v n l r) = toList(l) ++ (take n (repeat v)) ++ toList(r)


---------------------Тестові дані - Графи -------
gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]

---------------------Тестові дані - Дерева пошуку -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   