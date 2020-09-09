module Maybe where

{-

Below you can see that some functions don't always output a clear thing. 
They "might" output that thing.

ifEvenAdd2 :: Integer -> Integer 
ifEvenAdd2 n =
    if even n then n + 2 else ???
-}


-- here is the correct way to do that.
ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing

-- Smart Constructors for Datatypes

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

-- example smart constructor. 
-- only constructs the type with
-- certain criteria.
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age | name /= "" && age >= 0 = Just $ Person name age
                  | otherwise              = Nothing
