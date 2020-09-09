module PointFree where

{-
The idea seems to be just passing around functions without arguments and then 
passing in the arguments you need when you call your new mega-function. 

so (f . g) without any arguments applied. It can be useful with things like 
(f . g . h . z). 

-}

f :: Int -> [Int] -> Int
f z xs = foldr (+) z xs

f2 :: Int -> [Int] -> Int
f2 = foldr (+)
----------

f3 :: [Char] -> Int
f3 = length . filter (== 'a')
