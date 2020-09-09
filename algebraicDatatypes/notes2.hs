module Notes2 where

import           Data.List

data GuessWhat =
    ChickenButt deriving (Eq, Show)

data Id a =
    MkId a deriving (Eq, Show)

data Product a b =
    Product a b deriving (Eq, Show)

data Sum a b = First a
            | Second b
            deriving (Eq, Show)

trivialValue :: GuessWhat
trivialValue = ChickenButt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool
type Name = String

-- Here is person is both a name and an awesome value
-- it is the combination of both
person :: Product Name Awesome
person = Product "Simon" True


data Twitter =
    Twitter deriving (Eq, Show)

data AskFm =
    AskFm deriving (Eq, Show)

-- The point is that below you can only have
-- either Twitter or AskFm as the answer.
socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

data RecordProduct a b =
    RecordProduct { pfirst :: a
                , psecond :: b }
                deriving (Eq, Show)


myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 32 0.0001

-- the reason to do this is just to make things
-- more obvious
myRecord2 :: RecordProduct Integer Float
myRecord2 = RecordProduct { pfirst = 42, psecond = 0.0001 }


data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript deriving (Eq, Show)


data Programmer =
    Programmer { os :: OperatingSystem
            , lang :: ProgLang }
            deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

-- you didn't think of this but it is such a good way of doing it. 
-- I didn't know you could create data like that.
allProgrammers :: [Programmer]
allProgrammers =
    [ Programmer os lang | os <- allOperatingSystems, lang <- allLanguages ]
