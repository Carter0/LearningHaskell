module Patterns where

newtype Username =
    Username String

newtype AccountNumber =
    AccountNumber Integer

data User =
    UnregisteredUser
    | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"

-- The $ puts parens after it. So putStrLn (name ++ " " ++ show acctNum) same
printUser (RegisteredUser (Username name) (AccountNumber acctNum)) =
    putStrLn $ name ++ " " ++ show acctNum
-------------------------


 -- Penguin example
data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
    Peng WherePenguinsLive
    deriving (Eq, Show)


-- Below is pattern matching
-- it takes in a specific type of the data above and 
-- then returns something based on it.

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

-- this is a more general example of it.
gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

-- here is how you can match on two. 

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = (galapagosPenguin p) || (antarcticPenguin p)

 ---------

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

-- this is how you would have written it beforehand
-- f x y = ((snd x, snd y), (fst x, fst y))


----------

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

---------

funcZ :: (Eq a, Num a) => a -> [Char]
funcZ x = case x + 1 == 1 of
    True  -> "Awesome"
    False -> "What?"

pal :: Eq a => [a] -> [Char]
pal xs = case xs == reverse xs of
    True  -> "Yes"
    False -> "No"

-----------

functionC :: Ord p => p -> p -> p
functionC x y = case x > y of
    True  -> x
    False -> y

ifEvenAdd2 :: Integral p => p -> p
ifEvenAdd2 n = case even n of
    True  -> n + 2
    False -> n

nums :: (Ord a, Num a, Num p) => a -> p
nums x = case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0


-------

-- Getting into higher order functions

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

--------

data Employee =
    Coder
    | Manager
    | Veep
    | CEO
    deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' = case compare e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> reportBoss e' e

codersRuleCEOsDrule :: Employee -> Employee -> Ordering
codersRuleCEOsDrule Coder Coder = EQ
codersRuleCEOsDrule Coder _     = GT
codersRuleCEOsDrule _     Coder = LT
codersRuleCEOsDrule e     e'    = compare e e'

---------

-- Guards and what they are in haskell

myAbs1 :: Integer -> Integer
myAbs1 x = if x < 0 then (-x) else x

myAbs2 :: Integer -> Integer
myAbs2 x | x < 0     = (-x)
         | otherwise = x

bloodNa :: Integer -> String
bloodNa x | x < 135   = "too low"
          | x > 145   = "too high"
          | otherwise = "its good"

isRight :: (Eq a, Num a) => a -> a -> a -> String
isRight a b c | a ^ 2 + b ^ 2 == c ^ 2 = "Yuppers"
              | otherwise              = "Nopple"

dogYrs :: Integer -> Integer
dogYrs x | x == 0    = 0
         | x == 1    = 15
         | x == 2    = 30
         | otherwise = 70

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x | y >= 0.9  = 'A'
           | y >= 0.8  = 'B'
           | y >= 0.7  = 'C'
           | y >= 0.59 = 'D'
           | y < 0.59  = 'F'
    where y = x / 100

