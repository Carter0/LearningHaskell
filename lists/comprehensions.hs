module Comprehensions where

-- example comprehension that filters for vowels.
myString :: [Char] -> [Char]
myString xs = [ x | x <- xs, elem x "aeiou" ]

mySqr :: [Integer]
mySqr = [ x ^ 2 | x <- [1 .. 5] ]

myCube :: [Integer]
myCube = [ y ^ 3 | y <- [1 .. 5] ]

tuple :: [(Integer, Integer)]
tuple = [ (x, y) | x <- mySqr, y <- myCube ]

tuple2 :: [(Integer, Integer)]
tuple2 = [ (x, y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]

test :: Int
test = length ([ (x, y) | x <- mySqr, y <- myCube ])
