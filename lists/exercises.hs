module Exercises where

import           Data.Char

filterUpper :: [Char] -> [Char]
filterUpper = filter (\x -> isUpper x)

capitalize :: String -> String
capitalize []       = []
capitalize (x : xs) = toUpper x : xs

woot :: String -> String
woot []       = []
woot (x : xs) = toUpper x : woot xs

cap1 :: String -> Char
cap1 str = toUpper $ head str

cap2 :: [Char] -> Char
cap2 str = toUpper . head $ str

cap3 :: [Char] -> Char
cap3 = toUpper . head
