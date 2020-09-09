module Notes where

{-
A type is an "an enumeration of 
constructors that have zero or 
more arguments"

example

data Bool = False | True 

data : data keyword signalling we are making a 
data declaration

Bool: Type constructor. Takes no 
arguments

False and True: Both are nullary constructors.
Meaning they are data constructors that
take no arguments

| : this is "or" or the sum operator. 

example2

data [] a = [] | a : [a]

[] a: this is a type constructor that takes
one argument

[] : nullary data constructor

(a : [a]) : Data constructor that takes
two arguments. An a and a list of a.

True and False are also Type constants. As
they need no more information to be 
relevant as a type. We can call them constructors,
but they are not being constructed in any meaninful sense.


:k is short for Kind, which is like the 
next level up of a type. 

So, there are a lot of type words and data
words thrown around but

the stuff to the left of the equals is the 
type constructor, the stuff to the right is
the data constructors. 

:k Bool
Bool :: * means that it is a concrete type. 

:k []
[] :: * -> * means that it needs one concrete
type. That's why it is a constructor.

example

data PugType = PugData

PugType is a type constant because it has 
no arguments

PugData also has no arguments so it is 
also a constant value. 

example

data HuskyType a = HuskyData

HuskeyType is a type constructor with one
argument. But HuskyData doesn't do anything with
that a, so it is a phantom. 


data DogueDeBordeaux doge 
    = DogueDeBordeaux doge

The Data Constructor also has a doge. 
If the type constructor has a doge, 
then if the data constructor will have a 
variable, it must be a doge.
-}

data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge =
    DogueDeBordeaux doge

myPug :: PugType
myPug = PugData

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

-- Above works because 10 agrees with the type variable being bound
-- int 


--myDogeBad :: DogueDeBordeaux String
--myDogeBad = DogueDeBordeaux 10 -- doesnt work. 


data Doggies a
            = Husky a
            | Mastiff a
            deriving (Eq, Show)




{-


And this needs to be applied to become a concrete value:
Prelude> :t Husky
Husky :: a -> Doggies a
So, the behavior of constructors is such that if they don’t 
take any arguments, they behave like (type or value-level) constants. 
If they do take arguments, they act like (type or value-level) 
functions that don’t do anything except get applied.


-}


-- nullary
data Example0 =
  Example0 deriving (Eq, Show)
-- unary
data Example1 =
    Example1 Int deriving (Eq, Show)
-- product of Int and String
data Example2 =
    Example2 Int String deriving (Eq, Show)


{-
Cardinality : The amount of datatypes
your data can have. S

Example

data Bool = True | False
Cardinality of two 

Int8 has a cardinality of 256 because 2^8.

side note: 
Int is IntXX where XX is the bit width of the 
underlying architecture. 
Integer, on the other hand, is unbounded.


-}


{-
NewTypes

They can only ever have a single unary data 
constructor. It has no runtime overhead. 

Cannot be a product or sum type. Or contain 
nullary constructors. 

-}



newtype Goats =
    Goats Int deriving (Eq, Show)

newtype Cows =
    Cows Int deriving (Eq, Show)


tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42


-- remembers typeclasses are things like
-- eq and ord. We are making our own here.
-- It can take in any polymorphic arg
class TooMany a where
    tooMany :: a -> Bool

-- The instance of toomany of int is
-- is it less than 42.
instance TooMany Int where
    tooMany n = n > 42

instance TooMany Goats where
    tooMany (Goats n) = n > 43


{-
Sum Types

Basically, the | sign symbolizes a sum type.
You can just replace that with a + sign to get the
arity.
-}

-- An example sum type with arity 3
data QuantumBool =
    QuantumTrue
    | QuantumFalse
    | QuantomBoth
    deriving (Eq, Show)



{-
Product Types

These involve multiplication instead of addition. 
There is no special syntax for it.
-}

-- This has an arity of 9. (3 * 3)
data TwoQs =
    MkTwoQs QuantumBool QuantumBool
    deriving (Eq, Show)

{-
The reason why cardinality/arity is important is 
because the arity of a type roughly corresponds to 
how difficult it is to reason about.
-}

-- The arity of this is very large.
data Person =
    MkPerson String Int
    deriving (Eq, Show)

jm :: Person
jm = MkPerson "julie" 108
ca :: Person
ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s


{-
Record Constructors

The below record constructor does a very similar thing
to the datatype above, but it gives the "getters" by
default. 
-}


data Person2 =
    Person2 { name :: String
            , age :: Int }
            deriving (Eq, Show)


data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
            | NonfictionBook Nonfiction
            deriving Show

-- this is a type alias. 
-- It doesn't really do anything but change the name
type AuthorName = String

data Author = Author (AuthorName, BookType)

{-
Note, you can distribute products over sums.

So the Author type could be written as 

data Author =
    Fiction AuthorName
    | Nonfiction AuthorName deriving (Eq, Show)

The above is also now in normal form. As
nothing can be done to that until something
is filled in for those types.
-}


data GuessWhat =
    ChickenButt deriving (Eq, Show)

data Id a =
    MkId a deriving (Eq, Show)

data Product a b =
    Product a b deriving (Eq, Show)

data Sum a b = First a
            | Second b
            deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct { pfirst :: a
                , psecond :: b }
                deriving (Eq, Show)


newtype NumCow =
    NumCow Int
    deriving (Eq, Show)

newtype NumPig =
    NumPig Int
    deriving (Eq, Show)

data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

-- Both farmhouses are the same.


newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse =
    BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)


type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo =
    CowInfo Name Age
    deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LovesMud
    deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal =
    Cow CowInfo
    | Pig PigInfo
    | Sheep SheepInfo
    deriving (Eq, Show)


type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)
