module Exercises where


import           Control.Monad
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )



-- had no idea the exitSuccess method existed.
palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case (line1 == reverse line1) of
        True -> do
            putStrLn "It's a palindrome!"
            exitSuccess
        False -> putStrLn "Nope!"


-- you could do better, but I don't really care lol.
palindrome2 :: IO ()
palindrome2 = forever $ do
    line1 <- getLine
    let list = words line1
    case (list == reverse list) of
        True -> do
            putStrLn "It's a palindrome!"
        False -> putStrLn "Nope!"


type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
         NameEmpty
        | AgeTooLow
        | PersonInvalidUnknown String
        deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0
    = Right $ Person name age
    | name == ""
    = Left NameEmpty
    | not (age > 0)
    = Left AgeTooLow
    | otherwise
    = Left
        $  PersonInvalidUnknown
        $  "Name was: "
        ++ show name
        ++ " Age was: "
        ++ show age

-- good job here carter
gimmePerson :: IO ()
gimmePerson = do
    hSetBuffering stdout NoBuffering
    putStr "Enter the person's name: "
    name <- getLine
    putStr "Enter the persons age"
    temp <- getLine
    let age = read temp :: Integer
    case mkPerson name age of
        Left personInvalid ->
            putStrLn $ "An error occured" ++ show personInvalid
        Right person ->
            putStrLn $ "Yay, succesfully got a person: " ++ show person
