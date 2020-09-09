module WordNumber where

import           Data.List

digitToWord :: Int -> String
digitToWord n | n == 0    = "zero"
              | n == 1    = "one"
              | n == 2    = "two"
              | n == 3    = "three"
              | n == 4    = "four"
              | n == 5    = "five"
              | n == 6    = "six"
              | n == 7    = "seven"
              | n == 8    = "eight"
              | n == 9    = "nine"
              | otherwise = "uhhhhhh"

-- takes the integers and returns it as a list of integers
-- mod by 10 to gets the 10s place, div by 10 to reduce. No fractions in div.
digits :: Int -> [Int]
digits 0 = []
digits n = n `mod` 10 : digits (n `div` 10)


-- close enough lol
wordNumber :: Int -> String
wordNumber n = intersperse '-' (concat (map digitToWord (digits n)))
