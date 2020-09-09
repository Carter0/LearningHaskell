module Practice where

stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

{-
I completely forgot about comprehensions lol. 
I don't know how you would do this without them.
-}

svs :: [[Char]]
svs = [ [s1, v, s2] | s1 <- stops, v <- vowels, s2 <- stops ]

svsp :: [[Char]]
svsp = [ [s1, v, s2] | s1 <- stops, v <- vowels, s2 <- stops, s1 == 'p' ]

---------------

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny func list = foldr (||) False $ map func list

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 func = foldr (||) False . map func

myElem :: Eq a => a -> [a] -> Bool
myElem elem = foldr ((||) . (\x -> x == elem)) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 elem = any (\x -> x == elem)

-- im amazed I thought of this. 
-- I would not have been able to do that before
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap func = foldr ((:) . func) []

-- I looked this one up but im sad cause I was rly close
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then (a : b) else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap func = foldr ((++) . func) []

-- not falling for that id trick again!
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

