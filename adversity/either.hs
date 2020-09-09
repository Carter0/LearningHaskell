module Either where

{-
data Either a b = Left a | Right b 

-}

-- Smart Constructors for Datatypes

type Name = String
type Age = Integer


data Person = Person Name Age deriving Show


data PersonInvalid = NameEmpty
                    | AgeTooLow
                    deriving (Eq, Show)

{-

Below works well, but it has one problem. There
is no way to show a list of errors. So you
can't get the NameEmpty and AgeTooLow errors at the 
same time.

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age | name /= "" && age >= 0 = Right $ Person name age
                  | name == ""             = Left NameEmpty
                  | otherwise              = Left AgeTooLow

-}


-- Either a list of problems, or something else.
type ValidatePerson a = Either [PersonInvalid] a



ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
    True  -> Right age
    False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
    True  -> Right name
    False -> Left [NameEmpty]

-- never seen this before, its interesting.
-- still wrapping my head around how to use types.
mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

-- the number of pattern matching arguments correspond to the 
-- arity of the data.
mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOkay) (Right ageOkay) = Right (Person nameOkay ageOkay)
mkPerson' (Left  badName ) (Left  badAge ) = Left (badName ++ badAge)
mkPerson' (Left  badName ) _               = Left badName
mkPerson' _                (Left badAge)   = Left badAge
