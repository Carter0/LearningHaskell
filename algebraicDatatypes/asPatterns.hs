module AsPatterns where

import           Data.Char                      ( toUpper
                                                , ord
                                                , toLower
                                                , isUpper
                                                )

{-
As patterns allow you to pattern match on part of the statement and then
refer the whole original value.

You do this by using a binding (@) symbol. 
-}


f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
    print a
    return t


-- here this will double the value because XS isnt the rest, its the WHOLE list.
-- so this takes the first value and adds it to the whole list.
doubleUp :: [a] -> [a]
doubleUp []         = []
doubleUp xs@(x : _) = x : xs

-- true if one list has all the elements of the other
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _  = True
isSubseqOf _  [] = False
isSubseqOf full@(x : xs) (a : ab) =
    if x == a then isSubseqOf xs ab else isSubseqOf full ab


-- split and sentence into words then capitalize the latter one. 
--     Prelude> capitalizeWords "hello world"
--     [("hello", "Hello"), ("world", "World")]
capitalizeWords :: String -> [(String, String)]
capitalizeWords string = map capitalize $ words string
    where capitalize fullWord@(a : ab) = (fullWord, toUpper a : ab)

capitalizeWord :: String -> String
capitalizeWord []       = []
capitalizeWord (a : ab) = toUpper a : ab

-- nah, maybe if you want to do this some other time carter.
capitalizeParagraph :: String -> String
capitalizeParagraph string = undefined
