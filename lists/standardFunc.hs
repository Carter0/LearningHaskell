module StandardFunctions where

myOr :: [Bool] -> Bool
myOr []       = True
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f list = length (filter f list) > 0

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem elem (x : xs) | x == elem = True
                     | otherwise = myElem elem xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 elem list = myAny (== elem) list

myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []       = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f list = squish $ map f list

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

{-

Looked up the answer for two below. They are difficult to wrap 
your head around. They do double recursion. Sorta like a 
double loop, to make sure they test every element against another
if they need to.

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy [] = []
myMaximumBy f (x : xs) | 
-}

