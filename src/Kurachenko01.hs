{-# OPTIONS_GHC -Wall #-}
module Kurachenko01 where

integerTake :: Integer -> [Integer] -> [Integer]
integerTake 0 _ = []
integerTake _ [] = []
integerTake x (l:lt) = l:(integerTake (x-1) lt)

toPower :: Integer -> [Integer]
toPower x = x : (map (*x) (toPower x))

-- Task 1 -----------------------------------------
power3 :: [Integer]
power3 = [x ^ (3 :: Integer) | x <- [1::Integer ..]]

-- Task 2 -----------------------------------------
toPower3 :: [Integer]
--toPower3 = [3 ^ x | x <- [1::Integer ..]] --an old slow version
toPower3 = (3::Integer) : (map (* (3::Integer)) toPower3)

-- Task 3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 x = sum (integerTake x toPower3)

-- Task 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = sum (integerTake n (toPower m))

-- Task 5 -----------------------------------------
lessMe :: [Int] -> [Int]
lessMe ls = map (\x -> (length (filter (\y -> (y < x)) ls))) ls

-- Task 6 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]
frequency [] = []
frequency l@(lf:_) = (lf, (length (filter (\y -> (y == lf)) l))):(frequency (filter (\y -> (y /= lf)) l))

-- Task 7 -----------------------------------------
hailstone :: Int -> Int
hailstone x = if x `mod` 2 == 0
              then x `div` 2
              else 3 * x + 1

-- Task 8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq 1 = [1]
hailSeq x = x:(hailSeq (hailstone x))

-- Task 9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = map hailSeq [1..]

-- Task 10 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq x = (filter (\ls -> (length ls == x)) allHailSeq) !! 0 !! 0