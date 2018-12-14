{-# OPTIONS_GHC -Wall #-}
module Kurachenko03 where

data BinTree a = EmptyB
                | Node a (BinTree a) (BinTree a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a
               | Node2 (Tree23 a) a (Tree23 a)
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23
                   deriving (Eq, Show)

-- Utils --------------------------------------------

listSearch :: (Ord a) => BinTree a -> [a]
listSearch (EmptyB) = []
listSearch (Node v bt_1 bt_2) = (listSearch bt_1) ++ [v] ++ (listSearch bt_2)

list23 :: (Ord a) => Tree23 a -> [a]
list23 (Empty23) = []
list23 (Leaf v) = [v]
list23 (Node2 t1 _ t2) = (list23 t1) ++ (list23 t2)
list23 (Node3 t1 _ t2 _ t3) = (list23 t1) ++ (list23 t2) ++ (list23 t3)

-- Task 1 -------------------------------------------
isSearch :: (Ord a) => BinTree a -> Bool
isSearch (EmptyB) = True
isSearch (Node v bt_1@(Node v1 _ _) bt_2@(Node v2 _ _)) = (v > v1) && (v < v2) && (isSearch bt_1) && (isSearch bt_2)
isSearch (Node v bt_1@(Node v1 _ _) (EmptyB)) = (v > v1) && (isSearch bt_1)
isSearch (Node v (EmptyB) bt_2@(Node v2 _ _)) = (v < v2) && (isSearch bt_2)
isSearch (Node _ (EmptyB) (EmptyB)) = True

-- Task 2 -------------------------------------------
elemSearch :: (Ord a) => BinTree a -> a -> Bool
elemSearch (EmptyB) _ = False
elemSearch (Node v bt_1 bt_2) x = (v == x)
   || ( if x < v
        then elemSearch bt_1 x
        else elemSearch bt_2 x
   )

-- Task 3 -------------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a
insSearch (EmptyB) x = (Node x (EmptyB) (EmptyB))
insSearch bt@(Node v bt_1 bt_2) x = if x == v
                                 then bt
                                 else if x < v
                                      then (Node v (insSearch bt_1 x) bt_2)
                                      else (Node v bt_1 (insSearch bt_2 x))

-- Task 4 -------------------------------------------
delSearch :: (Ord a) => BinTree a -> a -> BinTree a
delSearch (EmptyB) _ = (EmptyB)
delSearch (Node v (EmptyB) bt_2) x = if x == v
                                     then bt_2
                                     else (Node v (EmptyB) (delSearch bt_2 x))
delSearch (Node v bt_1 (EmptyB)) x = if x == v
                                     then bt_1
                                     else (Node v (delSearch bt_1 x) (EmptyB))
delSearch (Node v bt_1 bt_2) x = if x == v
                                 then Node
                                      (head (listSearch bt_2))
                                      bt_1
                                      (delSearch bt_2 (head (listSearch bt_2)))
                                 else if x < v
                                      then (Node v (delSearch bt_1 x) bt_2)
                                      else (Node v bt_1 (delSearch bt_2 x))

-- Task 5 -------------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList ls = listSearch (foldl (insSearch) (EmptyB) ls)

-- Task 6 -------------------------------------------
isTree23 :: (Ord a) => Tree23 a -> Bool
isTree23 (Empty23) = True
isTree23 (Leaf _) = True
isTree23 (Node2 t_1 v t_2) = ((maximum (list23 t_1)) <= v)
    && ((head (list23 t_2)) == v)
    && (isTree23 t_1)
    && (isTree23 t_2)
isTree23 (Node3 t_1 v1 t_2 v2 t_3) = (v1 < v2)
    && ((maximum (list23 t_1)) <= v1)
    && ((maximum (list23 t_2)) <= v2)
    && ((head (list23 t_2)) == v1)
    && ((head (list23 t_3)) == v2)
    && (isTree23 t_1)
    && (isTree23 t_2)
    && (isTree23 t_3)

-- Task 7 -------------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 (Empty23) _ = False
elemTree23 (Leaf v) x = (v == x)
elemTree23 (Node2 t_1 v t_2) x = if x < v
                                 then elemTree23 t_1 x
                                 else elemTree23 t_2 x
elemTree23 (Node3 t_1 v1 t_2 v2 t_3) x = if x < v1
                                       then elemTree23 t_1 x
                                       else if x < v2
                                            then elemTree23 t_2 x
                                            else elemTree23 t_3 x

-- Task 8 -------------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 t_1 t_2 = (list23 t_1) == (list23 t_2)

-- ������ 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23  = undefined

-- isTerminal tr = True <=> ���� ���� ����� tr - ������ !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- ��������� ������� ����� � 2-3-������,
--   ����� ����� - ����� ���� Node2 ��� Node3 � ��`��� �� (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - ��������� ������� - ���� 2-3-������ a
--   : (a, Just (w, b)) - ��������� ������� ��� 2-3-������ a i b (w - �������� �������� � b)
--  insert v tr - ���� �������� v � ������ ������ tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr

-- insTerm v tr - �������� �������� v � ������ tr � ����� - ����������� �����
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insNode v tr - ���� �������� v � ������ tr � ������ - ������������� �����
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode = undefined

---  ������� ������
bt1, bt2 ::  BinTree Int
bt1 = Node 9 (Node 4 EmptyB
                     (Node 8 EmptyB EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB)
                      EmptyB)
bt2 = Node 9 (Node 4 EmptyB
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB)
                       EmptyB)

---- 2-3-������
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
                   (Node2 (Leaf 7) 8 (Leaf 8))
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )