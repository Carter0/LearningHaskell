module Show where 


{-
IO in haskell can have side effects, this is big bad in regards to lambda 
calculus. Not pure. So, there is a monad. Also, every function needs to 
return something, so the () means it returns an empty tuple.
-}

data Person = Person String deriving Show

printPerson :: Person -> IO ()
printPerson person = 
    putStrLn (show person)


data Mood = Blah
          | Woot 
          deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = 
    if x == Woot then 
        Blah
    else x


type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object deriving (Eq, Show)

s1 :: Sentence
s1 = Sentence "dogs" "drool" "dogs"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

