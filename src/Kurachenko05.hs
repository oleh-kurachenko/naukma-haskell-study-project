{-# OPTIONS_GHC -Wall #-}
module Kurachenko05 where

import Text.Printf

data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Show, Eq)

-- Task 1 -------------------------------------------
instance Ord AbstractInteger where
   (<=) x y = (aiToInteger x) <= (aiToInteger y)

-- Task 2 -------------------------------------------
aiToInteger :: AbstractInteger -> Integer
aiToInteger Zero = 0
aiToInteger (Succ x) = (aiToInteger x) + 1
aiToInteger (Pred x) = (aiToInteger x) - 1

-- Task 3 -------------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs Zero x = x
plusAbs (Succ x) (Pred y) = x + y
plusAbs (Pred x) (Succ y) = x + y
plusAbs (Succ x) y = x + (Succ y)
plusAbs (Pred x) y = x + (Pred y)

-- Task 4 -------------------------------------------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs Zero _ = Zero
timesAbs (Succ x) y = (x * y) + y
timesAbs (Pred x) y = (x * y) - y

-- Task 5 -------------------------------------------
negateAbs :: AbstractInteger -> AbstractInteger
negateAbs Zero = Zero
negateAbs (Succ x) = Pred (negateAbs x)
negateAbs (Pred x) = Succ (negateAbs x)

fromIntegerAbs :: Integer -> AbstractInteger
fromIntegerAbs 0 = Zero
fromIntegerAbs x = if (x > 0)
                   then (Succ (fromIntegerAbs (x - 1)))
                   else (Pred (fromIntegerAbs (x + 1)))

instance Num AbstractInteger where
    (+)         = plusAbs
    (-)         = \x y -> x + (negate y)
    (*)         = timesAbs
    negate      = negateAbs
    fromInteger = fromIntegerAbs
    abs         = \x -> if x <= Zero then negate x else x
    signum      = \x -> if x == Zero
                        then 0
                        else if x < 0
                             then -1
                             else 1

-- Task 6 -------------------------------------------
factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- Task 7 -------------------------------------------
data Quaternion = Quaternion Double Double Double Double deriving (Eq)

instance Show Quaternion where
    show (Quaternion x i j k) = printf "%f%+fi%+fj%+fk" x i j k

-- Task 8 -------------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion (Quaternion x i j k) (Quaternion x2 i2 j2 k2) =
  (Quaternion (x + x2) (i + i2) (j + j2) (k + k2))

-- Task 9 -------------------------------------------
timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) =
  Quaternion (a1*a2 - b1*b2 - c1*c2 - d1*d2)
             (a1*b2 + a2*b1 + c1*d2 - d1*c2)
             (a1*c2 - b1*d2 + c1*a2 + d1*b2)
             (a1*d2 + b1*c2 - c1*b2 + d1*a2)

--- Task 10 ------------------------------------------
absQuaternion :: Quaternion -> Quaternion
absQuaternion (Quaternion a b c d) =
  Quaternion (sqrt (a*a + b*b + c*c + d*d)) 0 0 0

signumQuaternion :: Quaternion -> Quaternion
signumQuaternion (Quaternion 0 0 0 0) = 0
signumQuaternion qt@(Quaternion a b c d) =
  (Quaternion (a/qSqrt) (b/qSqrt) (c/qSqrt) (d/qSqrt))
    where qSqrt = (\(Quaternion aq _ _ _) -> aq) (absQuaternion qt)

instance Num Quaternion  where
    (+)   = plusQuaternion
    (*)   = timesQuaternion
    negate      = \x -> x * (Quaternion (-1) 0 0 0)
    fromInteger = \x -> (Quaternion (fromInteger x) 0 0 0)
    abs         = absQuaternion
    signum      = signumQuaternion
