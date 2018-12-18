{-# OPTIONS_GHC -Wall #-}
module HWP07 where

import qualified Data.Set as Set
import Data.Maybe

type Index = Int
data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Index, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Index, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- Utils --------------------------------------------

fst3 :: (a, b, c) -> a
fst3 (v, _, _) = v
snd3 :: (a, b, c) -> b
snd3 (_, v, _) = v
trd3 :: (a, b, c) -> c
trd3 (_, _, v) = v

-- Task 1 -------------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat (node, bddNodes) envs =
  case node of
    0 -> False
    1 -> True
    _ -> checkSat (nextNode, bddNodes) envs where
           bddNode = fromJust (lookup node bddNodes)
           varValue = fromJust (lookup (fst3 bddNode) envs)
           nextNode = if varValue
                      then (trd3 bddNode)
                      else (snd3 bddNode)

-- Task 2 -------------------------------------------
listNodes :: [BDDNode] -> (Set.Set Int)
listNodes [] = Set.empty
listNodes (x:xs) = Set.insert (fst3 (snd x)) (listNodes xs)

allEnvs :: [Int] -> [Env]
allEnvs [] = [[]]
allEnvs (x:xs) = (map (\ls -> (x, True):ls) nextGen) ++
            (map (\ls -> (x, False):ls) nextGen)
              where nextGen = allEnvs xs

sat :: BDD -> [[(Index, Bool)]]
sat bdd = filter (\x -> checkSat bdd x)
                 (allEnvs (Set.toList (listNodes (snd bdd))))

-- Task 3 -------------------------------------------
simplify :: BExp -> BExp
simplify (Not (Prim x)) = Prim (not x)
simplify (And (Prim x) (Prim y)) = Prim (x && y)
simplify (Or (Prim x) (Prim y)) = Prim (x || y)
simplify x = x

-- Task 4 -------------------------------------------
restrict :: BExp -> Index -> Bool -> BExp
restrict (Prim x) _ _ = Prim x
restrict (IdRef inx) targetInx value =
  if inx == targetInx
  then Prim value
  else IdRef inx
restrict (Not bExp) targetInx value =
  simplify (Not (restrict bExp targetInx value))
restrict (And bExp1 bExp2) targetInx value =
  simplify (And (restrict bExp1 targetInx value)
                (restrict bExp2 targetInx value))
restrict (Or bExp1 bExp2) targetInx value =
  simplify (Or (restrict bExp1 targetInx value)
               (restrict bExp2 targetInx value))

-- Task 5 -------------------------------------------
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs = buildBDD' e 2 xs

buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' bExp _ [] = (if x then 1 else 0, [])
  where (Prim x) = restrict bExp 0 True
buildBDD' bExp nodeId (ref:refs) =
  if (length refs) == 0
  then (nodeId, [(nodeId, (ref,
                  if (restrict bExp ref False) == (Prim False) then 0 else 1,
                  if (restrict bExp ref True) == (Prim False) then 0 else 1))])
  else
    (nodeId, [(nodeId, (ref, nodeId * 2 - 1, nodeId * 2))]
      ++ (snd falseNode) ++ (snd trueNode))
      where
        falseNode = buildBDD' (restrict bExp ref False) (nodeId * 2 - 1) refs
        trueNode = buildBDD' (restrict bExp ref True) (nodeId * 2) refs


-- Task 6 -------------------------------------------

replaceRedundantNodeIds :: [NodeId] -> NodeId -> BDDNode -> BDDNode
replaceRedundantNodeIds redundantNodeIds replNodeId (nId, nodeContent) =
   (nId, (fst3 nodeContent,
          if elem (snd3 nodeContent) redundantNodeIds
          then replNodeId
          else (snd3 nodeContent),
          if elem (trd3 nodeContent) redundantNodeIds
          then replNodeId
          else (trd3 nodeContent)))

isRedundant :: BDDNode -> BDDNode -> Bool
isRedundant (nodeId, nodeContent) (nodeId2, nodeContent2) =
  (nodeId /= nodeId2) && (nodeContent == nodeContent2)

checkLayerDuplicates ::  BDDNode -> BDD -> BDD
checkLayerDuplicates node@(nodeId, _) (rootNodeId, bdds) =
  let
    redundantNodes = filter (\x -> isRedundant node x) bdds
    redundantNodeIds = map (\(nId, _) -> nId) redundantNodes
  in
    (rootNodeId, filter (\x -> not (isRedundant node x))
       (map (\x -> replaceRedundantNodeIds redundantNodeIds nodeId x) bdds))

-- no recursive call yet!
removeRedundant :: Index -> BDD -> BDD
removeRedundant 0 bdd = bdd
removeRedundant 1 bdd = bdd
removeRedundant inx bdd = -- (nodeId, bddNodes)
  let
    node = fromJust (lookup inx (snd bdd))
    leftNodeInx = snd3 node
    rightNodeInx = trd3 node
    leftCleaned =
      if leftNodeInx > 1
      then checkLayerDuplicates
           (leftNodeInx, fromJust (lookup leftNodeInx (snd bdd))) (removeRedundant leftNodeInx bdd)
      else bdd
    rightCleaned =
      if rightNodeInx > 1
      then case (lookup rightNodeInx (snd leftCleaned)) of
        Nothing -> leftCleaned
        Just x -> checkLayerDuplicates (rightNodeInx, x) (removeRedundant rightNodeInx leftCleaned)
      else leftCleaned
  in rightCleaned

removeDuplicateNode :: NodeId -> NodeId -> BDD -> BDD
removeDuplicateNode nodeFrom nodeTo (root, bddNodes) =
  let
    bddT = (if root == nodeFrom then nodeTo else root,
            map (\x -> replaceRedundantNodeIds [nodeFrom] nodeTo x) bddNodes)
  in
    (fst bddT, filter (\(nId, _) -> nId /= nodeFrom) (snd bddT))

removeDuplicate :: Index -> BDD -> BDD
removeDuplicate 0 bdd = bdd
removeDuplicate 1 bdd = bdd
removeDuplicate inx bdd =
  let
    node = --fromJust (lookup inx (snd bdd))
      case (lookup inx (snd bdd)) of
        Nothing -> error("Error inx = " ++ show inx ++ ", state = " ++ show bdd)
        Just x -> x
  in
    if (snd3 node == trd3 node)
    then
      removeDuplicate (snd3 node)
                      (removeDuplicateNode inx (snd3 node) bdd)
    else
      removeDuplicate (trd3 node) (removeDuplicate (snd3 node) bdd)

buildROBDD :: BExp -> [Index] -> BDD
buildROBDD bExp inx =
  let
    bdd = (buildBDD bExp inx)
    bddRR = removeRedundant (fst bdd) bdd
  in removeDuplicate (fst bddRR) bddRR

------------------------------------------------------
-- �������� ��� ����������..

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))
b9 = And (IdRef 3) (Or (IdRef 2) (And (Not (IdRef 2)) (IdRef 1)))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])
bdd9 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,0,0)),(9,(3,0,1)),(5,(2,10,11)),(10,(3,0,1)),(11,(3,0,1))])
