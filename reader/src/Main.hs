module Main where

import Control.Applicative (liftA2)
import Data.Char ( toUpper, toUpper )

main :: IO ()
main = do
  putStrLn "hello world"


boop :: Integer -> Integer
boop = (*2)

doop :: Integer -> Integer
doop = (+10)


bip :: Integer -> Integer
bip = boop . doop

anotherBip :: Integer -> Integer
anotherBip = fmap boop doop

{-
 - This is a little confusing above. Because it is unclear
 - what the "functoral context is for this". 
 - By functoral context, the book means the datatype that it is 
 - trying to lift the function over. 
 -
 - The context here is a partially applied function. Fmap composes
 - the two functions before applying them to an argument.
 -
 - Essentially
 -
 - fmap boop doop x == (*2) ((+10) x)
 - You can see that it needs one argument in order to be applied
 - Fmap for (-> a) is basically (.) or the composition operator.
 - -}

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

{-
 - bbop does something like ... 
 - ((+) <$> (*2) <*> (+10)) 3
 - (3*2) + (3+10)`
 -
 - -}
 

test :: Integer -> Integer
test = (+) <$> (*2) <*> (+10) 

{- 
 - For now the book just says how we would use the applicative 
 - here for when we have two functions share the same input 
 - and we want to do something to the final result.
 -}

{-
 - This is the same thing as above, but this time in a monadic
 - context
 -
 - So here are examples of functor, applicative, and monad 
 - for partially applied functions
 -}
boopDoop :: Integer -> Integer 
boopDoop = do
  a <- boop
  b <- doop 
  return (a + b)

{-
 - This is the idea of Reader. It is a way of stringing functions
 - together when all those functions are awaiting one input 
 - from a shared environment. 
 -
 - We’re going to get into the details of how it works, but 
 - the important intuition here is that it’s another way of 
 - abstracting out function application, and it gives us a way 
 - to do computation in terms of an argument that hasn’t 
 - been supplied yet.
 -
 - We use this most often when we have a constant value that 
 - we will obtain from somewhere outside our program that 
 - will be an argument to a whole bunch of functions. Using 
 - Reader allows us to avoid passing that argument around 
 - explicitly
 -}

 -- Examples
 
cap :: [Char] -> [Char] 
cap = map toUpper 

rev :: [Char] -> [Char] 
rev = reverse 

composed :: [Char] -> [Char] 
composed = cap . rev

fmapped :: [Char] -> [Char] 
fmapped = cap <$> rev

-- >>> tupled "happy"
-- ("HAPPY","yppah")
tuplede :: [Char] -> ([Char], [Char])
tuplede = (,) <$> rev <*> cap

-- >>> tupled' "banana"
-- ("ananab","BANANA")
tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (,) rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = do 
  a <- cap 
  b <- rev
  return (a, b)

-- I think this one was kinda tricky
tupled2 :: [Char] -> ([Char], [Char])
tupled2 = 
  cap >>= (\x -> rev >>= (\y -> return (x,y)))

{-
 - A reminder about functors and the types they take. 
 - A functor takes a * -> * value. So for a type like Maybe/Either
 - , one of the values has been prefilled/gets lifted 
 - over and the other gets transformed.
 -
 - The same thing is true of the (->) value. Where you need to 
 - have one of those arguments already applied. 
 - It is typically called r, but it can be anything.
 - So (-> r) would mean that r is the already applied value. 
 - Like (*2), where the 2 is r. 
 - -}

 {- The Reader Newtype
  -
  - newtype Reader r a = 
  -       Reader { runReader :: r -> a}
  -
  - The accessor here just allows you to quickly access the 
  - function inside the larger type. It's for convenience.
  - -}

newtype Reader r a = 
    Reader { runReader :: r -> a}

-- Reader contains a function inside
-- the function inside is an a -> a function (id)
ask :: Reader a a 
ask = Reader id

{- InDepth on Applicative for (-> r)
 -
 - Remember that :i (->) is 
 - data (->) (a :: TYPE q) (b :: TYPE r)
 - or for simplicities sake (not accuracies)
 - :t (->) is (->) a b
 - The arrow operator (->) is an infix operator, so
 - (->) a b = a -> b
 -
 - In Applicative f, the f is approx. (-> r), so 
 - pure :: a -> f a would be 
 - pure :: a -> ((->) r a) applying to
 - pure :: a -> (r -> a)
 -
 - (*) :: f (a -> b) -> f a -> f b
 - (*) :: ((->) r) (a -> b) -> ((->) r) a -> ((->) r b)
 - (*) :: (r -> a -> b) -> (r -> a) -> (r -> b)
 -
 - So (*) has two arguments, and both are functions awaiting an 
 - r as an input.
 -
 - -}

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)


data Person = Person {
humanName :: HumanName 
, dogName :: DogName
, address :: Address
} deriving (Eq, Show)

data Dog = Dog {
dogsName :: DogName
, dogsAddress :: Address 
} deriving (Eq, Show)

pers :: Person
pers = 
  Person 
    (HumanName "Big Bird") 
    (DogName "Barkley")
    (Address "Sesame Street")


chris :: Person
chris =
  Person 
    (HumanName "Chris Allen") 
    (DogName "Papu")
    (Address "Austin")

-- without Reader
getDog :: Person -> Dog 
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog 
getDogR = Dog <$> dogName <*> address

-- with Reader, alternative
getDogR' :: Person -> Dog 
getDogR' = liftA2 Dog dogName address

{- Above is an example of reader in action. 
 -
 - To be honest I think it just makes everything a little 
 - more confusing.
 -
 - -}

-- Exercises 2

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c 
myLiftA2 f g t = f <$> g <*> t

asks :: (r -> a) -> Reader r a 
asks = Reader

-- Implementing the Reader Applicative

instance Functor (Reader r) where 
  fmap f (Reader r) = Reader $ f . r

-- That apply function is a doozy. That was hard
instance Applicative (Reader r) where 
  pure a = Reader $ const a
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r) 

{-
 - I just don't really see the point of reader. It seems to make everything else pointlessly complicated
 - -}























