{-# OPTIONS_GHC -Wall #-}
module WHP06 where

import Data.Maybe
import Data.List
--import Debug.Trace
import qualified Data.Map as M

-- Всі програми і їх елементи являються вірними (well-formed) в наступному значенні:
--   Всі оператори, функції і процедури застосовуються
--      до вірної кількості аргументів, кожний з яких має відповідний тип.
--   Вирази, які повинні обчислювати логічні значення, будуть завжди обчислюватися
--     до 0 (false) або 1 (true).
--   В присвоєнні масиву  a[i] = e масив завжди визначений (в області дії).
--   Процедура, що викликається як x := p(e1, …, en), завжди буде повертати значення
--     (закінчує своє обчислення оператором return e)
--   Оператор return завжди останній оператор для виконання в блоку процедури
--     (тобто немає "мертвого коду")

--------------------------------------------------------------------
type Id = String
data Value = I Int | A [(Int, Int)]
           deriving (Eq, Show)
data Op = Add | Minus | Mul | Less | Equal | Index
          deriving (Eq, Show)
data Exp = Const Value |
           Var Id |
           OpApp Op Exp Exp |
           Cond Exp Exp Exp |
           FunApp Id [Exp]
         deriving (Eq, Show)

data VarDef = Arr Id | Int Id   deriving (Eq, Show)
type FunDef = (Id, ([VarDef], Exp))

type Binding = M.Map Id Value
type StateP = ([Binding],Binding)
-- st = ([locn,.. loc1], glob)  стек локальних записів активацій + глобальний запис активації

data Statement = Assign Id Exp |
                 AssignA Id Exp Exp |
                 If Exp Block Block |
                 While Exp Block |
                 Call Id Id [Exp] |
                 Return Exp
               deriving (Eq, Show)

type Block     = [Statement]
type ProcDef   = (Id, ([VarDef], Block))
type Program   = ([VarDef], [FunDef], [ProcDef])

-- Task 1 -------------------------------------------
getValue ::  Id -> StateP -> Value
getValue valueId stateP =
  case M.lookup valueId (M.union (getGlobals stateP) (getLocals stateP)) of
    Nothing -> error ("Value Id not found: " ++ show valueId )
    Just x -> x

-- Task 2 -------------------------------------------
getLocals :: StateP -> Binding
getLocals stateP = head (fst stateP)

getGlobals :: StateP -> Binding
getGlobals stateP = snd stateP

-- Task 3 -------------------------------------------
assignArray :: Value -> Value -> Value -> Value
assignArray (A arr) (I index) (I newValue) =
  case find (\(arrIndex, _) -> arrIndex == index) arr of
    Nothing -> A ((index, newValue):arr)
    Just _ -> A (map (\(arrIndex, arrValue) ->
      if arrIndex == index then (arrIndex, newValue) else (arrIndex, arrValue))
        arr)
assignArray _ _ _ = error ("assignArray called with bad parameters")

-- Task 4 -------------------------------------------
updateVar :: (Id, Value) -> StateP -> StateP
--updateVar (valueId, value) stateP@(local:tailLocal, global)
--  | trace ("\n->call " ++ show (valueId, value) ++ "|" ++ show stateP)
-- False = undefined
updateVar (valueId, value) (local:tailLocal, global) =
--  if valueId == ""
--  then stateP
--  else
  case M.member valueId global of
        False -> ((M.insert valueId value local):tailLocal, global)
        True -> (local:tailLocal, (M.insert valueId value global))
updateVar (valueId, value) ([], global) =
--  if valueId == ""
--  then stateP
--  else
  case M.member valueId global of
        False -> ([(M.insert valueId value M.empty)], global)
        True -> ([], (M.insert valueId value global))
--updateVar _ _ = error ("updateVar called with bad parameters")
--updateVar par1 par2 =
--  error ("updateVar called with bad parameters: " ++ show par1 ++ "|" ++ show par2)

-- Task 5 -------------------------------------------
applyOp :: Op -> Value -> Value -> Value
-- Передумова: Значення мають відповідні типи (I або A) для кожної операції
applyOp Add (I val1) (I val2) = (I (val1 + val2))
applyOp Minus (I val1) (I val2) = (I (val1 - val2))
applyOp Mul (I val1) (I val2) = (I (val1 * val2))
applyOp Less (I val1) (I val2) = (I (if val1 < val2 then 1 else 0))
applyOp Equal (I val1) (I val2) = (I (if val1 == val2 then 1 else 0))
applyOp Index (A arr) (I inx) = I (fromMaybe 0 (lookup inx arr))
applyOp _ _ _ = error ("applyOp called with bad parameters")


-- Task 6 -------------------------------------------
bindArgs :: [Id] -> [Value] -> Binding
-- Передумова: списки мають однакову довжину
bindArgs (idHead:ids) (valueHead:values) =
  M.insert idHead valueHead (bindArgs ids values)
bindArgs [] [] = M.empty
bindArgs _ _ = error ("bindArgs called with bad parameters")

-- Task 7 -------------------------------------------
dropType :: VarDef -> Id
dropType (Arr varId) = varId
dropType (Int varId) = varId

eval :: Exp -> [FunDef] -> StateP -> Value
eval (Const value) _ _ = value
eval (Var varId) _ stateP = getValue varId stateP
eval (OpApp op exp1 exp2) funDefs stateP =
  applyOp op (eval exp1 funDefs stateP) (eval exp2 funDefs stateP)
eval (Cond expPredicate expTrue expFalse) funDefs stateP =
  case eval expPredicate funDefs stateP of
    I 1 -> eval expTrue funDefs stateP
    I 0 -> eval expFalse funDefs stateP
    _ -> error ("bad predicate value in eval Cond")
eval (FunApp funcId paramExprs) funDefs stateP =
  case find (\(fId, _) -> fId == funcId) funDefs of
    Nothing -> error ("Function not found: " ++ show funcId)
    Just (_, (varDefs, funcExp)) ->
      eval funcExp funDefs ((args:fst stateP), snd stateP)
        where args = bindArgs
                     (map dropType varDefs)
                     (evalArgs paramExprs funDefs stateP)

evalArgs :: [Exp] -> [FunDef] -> StateP -> [Value]
evalArgs exps funDefs stateP = map (\x -> eval x funDefs stateP) exps

-- Task 8 -------------------------------------------
executeStatement :: Statement -> [FunDef] -> [ProcDef] -> StateP -> StateP
executeStatement (Assign varId expr) funDefs _ stateP =
  updateVar (varId, eval expr funDefs stateP) stateP
executeStatement (AssignA varId inxExp valExp) funDefs _ stateP =
  updateVar (varId, newVarValue) stateP
    where newVarValue = assignArray
                        (getValue varId stateP)
                        (eval inxExp funDefs stateP)
                        (eval valExp funDefs stateP)
executeStatement (If expPredicate blockTrue blockFalse)
                 funDefs
                 procDefs
                 stateP =
  case eval expPredicate funDefs stateP of
     I 1 -> executeBlock blockTrue funDefs procDefs stateP
     I 0 -> executeBlock blockFalse funDefs procDefs stateP
     _ -> error ("bad predicate value in executeStatement If")
executeStatement (While expPredicate block) funDefs procDefs stateP =
  case eval expPredicate funDefs stateP of
     I 1 -> executeStatement (While expPredicate block) funDefs procDefs stRes
       where stRes = executeBlock block funDefs procDefs stateP
     I 0 -> stateP
     _ -> error ("bad predicate value in executeStatement While")
executeStatement (Call varId procId paramExps) funDefs procDefs stateP =
  case find (\(prId, _) -> prId == procId) procDefs of
      Nothing -> error ("Procedure not found: " ++ show procId)
      Just (_, (varDefs, procBlock)) ->
        if varId == ""
        then (tail (fst callResultState), snd callResultState)
        else updateVar (varId, (getValue "$res" callResultState)) (tail (fst callResultState), snd callResultState)
        --error ("Res state: " ++ show stateP)
          where callResultState = executeBlock procBlock
                                  funDefs
                                  procDefs
                                  ((args:fst stateP), snd stateP)
                args = bindArgs
                       (map dropType varDefs)
                       (evalArgs paramExps funDefs stateP)
executeStatement (Return expr)
                 funDefs
                 _
                 stateP@((local:localTail), global) =
  (((M.insert "$res" exprValue local):localTail), global)
    where exprValue = eval expr funDefs stateP
executeStatement (Return _) _ _ stateP =
  error ("bad Return, state: " ++ show stateP)

executeBlock :: Block -> [FunDef] -> [ProcDef] -> StateP -> StateP
executeBlock (statement:statements) funDefs procDefs stateP =
  executeBlock statements
               funDefs
               procDefs
               (executeStatement statement funDefs procDefs stateP)
executeBlock [] _ _ stateP = stateP

---------------------------------------------------------------------
-- Допоміжні функції і дані для тестування...
-- Функція пошуку...
lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t = fromMaybe (error ("\nНе знайдено  " ++ show x ))
              (lookup x t)

-- Стан для тестування
sampleState :: StateP
sampleState = ([M.fromList [("x",I 5)]], M.fromList [("y",I 2),("a", listToVal [4,2,7])])

-- Перетворює список цілих в масив Value...
listToVal :: [Int] -> Value
listToVal xs = A (zip [0..] xs)

 -- Перетворює ціле в Exp...
intToExp :: Int -> Exp
intToExp n = Const (I n)

-- Реалізація виконання програми
program :: Program -> StateP
program (dvx, dfx, dpx) =
   let initv :: VarDef -> (Id, Value)
       initv (Arr v) = (v, A [])
       initv (Int v) = (v, I 0)
       gl = M.fromList (map initv dvx)
   in executeStatement (Call "" "main" []) dfx dpx ([],gl)

-- fib чиста функція
-- Функція fib, що обчислює число Фібоначчі
-- func  fib(n) =
--     (n < 3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
     ([Int "n"], Cond (OpApp Less (Var "n") (Const (I 3)))
                  (Const (I 1))
                  (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const (I 1))])
                             (FunApp "fib" [OpApp Minus (Var "n") (Const (I 2))]))
     )
    )

-- Масив
sampleArray :: Exp
sampleArray = Const (listToVal [9,5,7,1])

-- Сума елементів масиву 0..n ...
sumA1 :: ProcDef
sumA1 = ("sumA1",
     ([Arr "a", Int "n"],
                  [Assign "s" (Const (I 0)),
                   Assign "i" (Const (I 0)),
                   Assign "limit" (OpApp Add (Var "n") (Const (I 1))),
                   While (OpApp Less (Var "i") (Var "limit"))
                         [Assign "s" (OpApp Add (Var "s")
                                                (OpApp Index (Var "a") (Var "i"))),
                          Assign "i" (OpApp Add (Var "i") (Const (I 1)))
                         ],
                   Return (Var "s")]
     )
    )

-- Додавання двох чисел...
gAdd :: ProcDef
gAdd = ("gAdd",
     ([Int "x", Int "y"], [Assign "gSum" (OpApp Add (Var "x") (Var "y"))])
    )

-- Повна програма
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],[Call "" "gAdd" [intToExp 5, intToExp 10] ]))])
