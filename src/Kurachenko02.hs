{-# OPTIONS_GHC -Wall #-}
module Kurachenko02 where

-- Task 1 --------------------------------------------
sumFl :: [Integer] -> Integer
sumFl [] = 0
sumFl (x:xs) = foldl (+) x xs

-- Task 2 --------------------------------------------
productFr :: [Integer] -> Integer
productFr [] = 1
productFr xs = foldr (*) 1 xs

-- Task 3 --------------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr fs ss = foldr (\x ls -> (x:ls)) ss fs

-- Task 4 --------------------------------------------
sortInsert :: [Int] -> [Int]
sortInsert ls = foldl
  (\il x -> (filter (\y -> y < x) il) ++ [x] ++ (filter (\y -> y >= x) il))
  []
  ls

-- Task 5 --------------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int]
findIndices prd ls = foldl
  (\is (x, _) -> is ++ [x])
  []
  (filter (\(_, y) -> prd y) (zip [0::Int ..] ls))

-- Task 6 --------------------------------------------
allReverse :: [String] -> [String]
allReverse ls = map reverse (reverse ls)

-- Task 7  -------------------------------------------
noDigits :: String -> String
noDigits ls = filter (\x -> not (elem x ['0'..'9'])) ls

-- Task 8 --------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ls x = length (filter (\y ->y x) ls)

-- Task 9 --------------------------------------------
trianglePas :: [[Integer]]
trianglePas = iterate (\x -> zipWith (+) ([0] ++ x) (x ++ [0])) [1]

-- Task 10 -------------------------------------------
factorialsM :: [Integer]
factorialsM = zipWith (\x y -> foldl (*) x [1..y]) [1, 1..] [1..]