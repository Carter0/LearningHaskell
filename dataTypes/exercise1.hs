module Exercise1 where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

awesome :: [[Char]]
awesome = ["Papuchon", "curry", ":)"]

also :: [[Char]]
also = ["Quake", "The Simons"]

allAwesome :: [[[Char]]]
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs int = if int > 0 then int else negate int

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f tup1 tup2 = ((snd tup1, snd tup2), (fst tup1, fst tup2))


x = (+)

f2 xs = x w 1 where w = length xs

f3 x = x
