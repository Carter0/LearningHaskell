module List1 where


safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (_ : []) = Nothing
safeTail (_ : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []  = Nothing
safeHead [x] = Just x

eftBool :: Bool -> Bool -> [Bool]
eftBool bool1 bool2 = enumFromTo bool1 bool2


eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd ord1 ord2 = enumFromTo ord1 ord2

eftInt :: Int -> Int -> [Int]
eftInt int1 int2 = enumFromTo int1 int2


eftChar :: Char -> Char -> [Char]
eftChar char1 char2 = enumFromTo char1 char2

-- takeWhile (/= ' ') "man is banana"

badSplitString :: String -> [String]
badSplitString []     = []
badSplitString string = takeWhile (/= ' ') trim
    : badSplitString (dropWhile (/= ' ') trim)
    where trim = dropWhile (== ' ') string

-- this is what map is doing under the hood. 
-- the thing to keep in mind is that arguments are not 
-- evaluated until map goes back up the list. 
-- it goes all the way down, putting f to each item, 
-- then goes back up applying f.
myMap :: (a -> b) -> [a] -> [b]
myMap _ []       = []
myMap f (x : xs) = f x : myMap f xs


-- lazy evaluation tidbit. 
{-
Since haskell uses lazy evaluated, you can get away with incomplete
things. Like fst(1, undefined) actually will evaluate to 1. 
Because it would never need to see what undefined is.
-}

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x : xs) | pred x    = x : myFilter pred xs
                       | otherwise = myFilter pred xs

filter30 :: [Integer]
filter30 = filter (\x -> rem x 3 == 0) [1 .. 30]

lengthFilter30 :: Int
lengthFilter30 = length . filter (\x -> rem x 3 == 0) $ [1 .. 30]

filterStuff :: String -> [String]
filterStuff string = filter (\x -> not (elem x ["the", "a", "an"])) stringList
    where stringList = words string


myZip :: [a] -> [b] -> [(a, b)]
myZip []       []       = []
myZip _        []       = []
myZip []       _        = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ []       []       = []
myZipWith f _        []       = []
myZipWith f []       _        = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys
