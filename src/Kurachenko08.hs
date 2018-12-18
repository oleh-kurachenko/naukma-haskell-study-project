{-# OPTIONS_GHC -Wall #-}
module Kurachenko08 where

import Data.Array
import Data.List

type Graph = Array Int [Int]


-- Task 1 --------------------------------------------
longWayDfs :: Graph -> [Int] -> Int -> Int -> [Int]
longWayDfs graph visited current target =
  if (current == target)
  then visited
  else let
         ways = map (\v ->
                  if not (elem v visited)
                  then longWayDfs graph (visited ++ [v]) v target
                  else []) (graph ! current)
       in foldl (\x y -> if (length x > length y) then x else y) [] ways

longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay graph from to =
  let way = longWayDfs graph [from] from to
  in if way == [] then Nothing else Just way

-- Task 2 -------------------------------------------
isCycleDfs :: Graph -> [Int] -> Int -> Bool
isCycleDfs graph visited current =
  let
    ways = map (\v ->
             if not (elem v visited)
             then isCycleDfs graph (visited ++ [v]) v
             else True) (graph ! current)
  in foldl (\x y -> x || y) False ways

isNoCycle :: Graph -> Bool
isNoCycle graph =
  if length (indices graph) == 0
  then True
  else not (isCycleDfs graph [root] root)
    where root = head (indices graph)

-- Task 3 -------------------------------------------
isTransitive :: Graph -> Bool
isTransitive graph =
  let pairs = [(x, y) | x<-(indices graph), y<-(indices graph),
                x /= y && (longWay graph x y) /= Nothing ]
  in foldl (\x (v1, v2) -> x && (elem v2 (graph ! v1))) True pairs

-- Task 4 -------------------------------------------
isGraph :: Graph -> Bool
isGraph graph =
  let pairs = [(x, y) | x<-(indices graph), y<-(indices graph),
                x /= y && (elem x (graph ! y)) ]
  in foldl (\x (v1, v2) -> x && (elem v2 (graph ! v1))) True pairs

-- Task 5 -------------------------------------------

shortWayDfs :: Graph -> [Int] -> Int -> Int -> [Int]
shortWayDfs graph visited current target =
  if (current == target)
  then visited
  else
    let
      ways = map (\v ->
               if not (elem v visited)
               then shortWayDfs graph (visited ++ [v]) v target
               else (0:(indices graph))) (graph ! current)
    in foldl (\x y -> if (length x < length y) then x else y)
             (0:(indices graph)) ways

shortWay :: Graph -> Int -> Int -> Maybe [Int]
shortWay graph from to =
  let way = shortWayDfs graph [from] from to
  in if way == (0:(indices graph)) then Nothing else Just way


-- Task 6 -------------------------------------------
isConnecting :: Graph -> Bool
isConnecting graph =
  let pairs = [(x, y) | x<-(indices graph), y<-(indices graph), x /= y]
  in foldl (\x (v1, v2) -> x && (longWay graph v1 v2 /= Nothing)) True pairs

-- Task 7 -------------------------------------------
componentsDfs :: Graph -> [Int] -> Int -> [Int]
componentsDfs graph visited current =
  let
    ways = map (\v ->
             if not (elem v visited)
             then componentsDfs graph (visited ++ [v]) v
             else visited) (graph ! current)
  in foldl (\x y -> nub (x ++ y)) visited ways

componentsUnviewed :: Graph -> [Int] -> [[Int]]
componentsUnviewed _ [] = []
componentsUnviewed graph (x:xs) =
  (component:componentsUnviewed graph (xs \\ component))
    where component = componentsDfs graph [x] x

components :: Graph -> [[Int]]
components graph = componentsUnviewed graph (indices graph)

-- Task 8 -------------------------------------------
topolDfs :: Graph -> [Int] -> Int -> [Int]
topolDfs graph visited current =
  let
    ways = map (\v ->
             if not (elem v visited)
             then topolDfs graph (v:visited) v
             else visited) (graph ! current)
  in nub $ reverse (foldl (\x y -> (x ++ y)) visited ways)

topolUnviewed :: Graph -> [Int] -> [[Int]]
topolUnviewed _ [] = []
topolUnviewed graph (x:xs) =
  (component:topolUnviewed graph (xs \\ component))
    where component = topolDfs graph [x] x

topolSorting :: Graph -> Maybe[Int]
topolSorting graph =
  if isNoCycle graph
  then
    let allVerts = topolUnviewed graph (indices graph)
    in (Just (nub $ reverse (foldr union [] allVerts)))
  else
    Nothing
