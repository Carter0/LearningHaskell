module TypeMatching where 

import Data.List

-- Need Num a => thing
i :: Num a => a
i = 1

-- Need this to be float, Num a bad
f1 :: Float
f1 = 1.0

-- this works
f2 :: RealFrac a => a
f2 = 1.0

freud :: Ord a => a -> a
freud x = x

freud2 :: Int -> Int
freud2 x = x

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk function value1 value2 =
    value2 == (function value1)  

arith :: Num b => (a -> b) -> Integer -> a -> b
arith function int value =
    (function value) + fromInteger int 
