module Exercises where


notThe :: String -> Maybe String
notThe str = if str == "the" then Nothing else Just str

nothingCase :: Maybe String -> String
nothingCase Nothing    = "a"
nothingCase (Just str) = str

replaceThe :: String -> String
replaceThe sentence = unwords newSentence
 where
  sentenceList = words sentence
  newSentence  = map (nothingCase . notThe) sentenceList


-- this one took me a while lol
vowels :: String
vowels = "aeiou"

isFirstVowel :: String -> Bool
isFirstVowel str = elem (head str) vowels

nothingCase2 :: Maybe String -> String -> Integer
nothingCase2 Nothing    nextString = if isFirstVowel nextString then 1 else 0
nothingCase2 (Just str) _          = 0

-- cool case of recursion and pattern matching here.
recursiveCount :: [String] -> Integer
recursiveCount (fst : snd : rest) =
  nothingCase2 (notThe fst) snd + recursiveCount (snd : rest)
recursiveCount _ = 0

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = recursiveCount . words

countIsVowel :: Char -> Integer
countIsVowel char = if char `elem` vowels then 1 else 0

countVowels :: String -> Integer
countVowels word = sum $ map countIsVowel word

newtype Word' =
    Word' String deriving (Eq, Show)

countIsNotVowel :: Char -> Integer
countIsNotVowel char = if char `elem` vowels then 0 else 1

countNotVowels :: String -> Integer
countNotVowels word = sum $ map countIsNotVowel word

mkWord :: String -> Maybe Word'
mkWord sentence = if countVowels sentence > countNotVowels sentence
  then Nothing
  else Just $ Word' sentence


------------
-- As natural as any
-- competitive bodybuilder 
data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)


-- took me a bit to wrap my head around how the data was used.
{-
example = Succ Zero
natToInteger example = 1

example = Succ $ Succ $ Succ Zero
natToInteger example = 3
-}
natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat int | int < 0   = Nothing
                 | otherwise = Just $ integerToNatHelper int

integerToNatHelper :: Integer -> Nat
integerToNatHelper int | int == 0  = Zero
                       | otherwise = Succ (integerToNatHelper $ int - 1)

-------------

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just a) = True

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just a) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee base _ Nothing  = base
mayybee base f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe b Nothing  = b
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes list = concatMap removeMaybe2 list
 where
  removeMaybe2 maybe = case maybe of
    Nothing  -> []
    (Just a) -> [a]


--flipMaybe :: [Maybe a] -> Maybe [a]
--flipMaybe []         = Just []
--flipMaybe (mx : mxs) = case mx of
--    Nothing -> Nothing
--    Just a  -> a : flipMaybe mxs -- idk lol


------------

-- i really struggled with these folding functions
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
 where
  f either restOfEither = case either of
    Left  a -> a : restOfEither
    Right b -> restOfEither

rights' :: [Either a b] -> [b]
rights' = foldr f []
 where
  f either restOfEither = case either of
    Left  a -> restOfEither
    Right b -> b : restOfEither

-- this took a bit for me. I am not used to either.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr partitionFunction ([], [])
 where
  partitionFunction either (as, bs) = case either of
    Left  a -> ((a : as), bs)
    Right b -> (as, (b : bs))

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left  a) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f f2 (Left  a) = f a
either' f f2 (Right b) = f2 b

-------


{-

Prelude> take 10 $ iterate (+1) 0
[0,1,2,3,4,5,6,7,8,9]

f b = Just (b, b + 1)
Prelude> take 10 $ unfoldr f 0
[0,1,2,3,4,5,6,7,8,9]

-}

myIterate :: (a -> a) -> a -> [a]
myIterate func base = base : myIterate func (func base)

-- I did it!
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr func base = case func base of
  Nothing     -> []
  Just (a, b) -> a : myUnfoldr func b

-- don't know why this is better, but I did it lol.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\y -> Just (y, f y)) x


-----------


data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- wow good job carter.
unfoldTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldTree func base = case func base of
  Nothing         -> Leaf
  Just (a, b, a2) -> Node (unfoldTree func a) b (unfoldTree func a2)

-- yay!
treeBuild :: Integer -> BinaryTree Integer
treeBuild treeDepth = unfoldTree func 0
  where func n = if n == treeDepth then Nothing else Just (n + 1, n, n + 1)
