module Notes where

import Control.Applicative (liftA3)
import Data.List (elemIndex)

{-
The Applicative type class is a monoidal functor...
-}

class Functor f => Applicative2 f where
  pure2 :: a -> f a
  (<*>!) :: f (a -> b) -> f a -> f b

{-

  pure does something fairly simple here. It takes a value and wraps it in some structure.

  So for example if the applicative was Maybe it would be
    a -> Maybe a

  Since pure wraps something in a structure, you sometimes need to define exactly
  what structure pure is going to be wrapping it in.

  pure 1 :: [Int] -> [1]
  pure 1 :: Maybe Int -> Just 1

  Another interesting note.

  pure (*2) <*> [1, 2, 3] is equal to [(*2)] <*> [1, 2, 3] which is equal to fmap (*2) [1, 2, 3]

  Coming back to pure again a bit later.

  Think of pure as the identity function for applicative. Remember that the monoid in applicative
  is for the structure. So the identity for applicative leaves the structure alone.
-}

{-
<*> is the more involved of the two. It is short for apply or ap.

The way the book explains it is like this ...

<*> :: f (a -> b) -> f a -> f b

You are going to have a container of functions and a container of values as arguments to this function.
But, you need to output a container of different values. So, a functor can be used to deal with the
values, but you need some way of combining containers. Monoid can be used to combine types together, so
you use that to combine the containers.

($) :: (a -> b) -> a -> b
(<$>) :: (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b

($) I have this (a -> b) function and I want to apply it to this value.
(<$>) I have this a -> b function and I want to apply it to the value inside of the container
(<*>) I have this container around (a -> b) functions. Apply that to the values inside of the container
of a's and merge the containers together.
-}

{-

---------
Reminder about record constructors.

newType Sum a = Sum {getSum :: a}

is equivalent to

newType Sum a = Sum a

getSum :: Sum -> Int
getSum (Sum value) = value

also sum would have an instance of monoid among other instances.

instance Monoid a => Monoid (Sum a)
  mempty = 0
  <> = (+)

---------
-}

{-
The Tuple Applicative

instance Monoid a => Applicative ((,) a) where
  pure x = undefined
  (<*>) = undefined

The a value has to be a monoid in the instance of applicatives tuple.
The b value just needs the left side to be an a -> b function.
The monoids combine because, well, thats what monoids do. The b value relies on a function.

The monoid needs to exist for the a because the applicative, like functor, only takes into account the b
value of the tuple initially. Remember, the applicative class declaration declares the type taken in
to be a functor. A.K.A, an (a -> b) function. A tuple is an a -> b -> (a, b) sorta thing.
But with an applicative you can't just leave the a type alone. So, the applicative
has a monoid constraint around the a to combine them.

Prelude> (Sum 2, (+1)) <*> (Sum 0, 0)
(Sum {getSum = 2},1)

Prelude> (Product 3, (+9))<*>(Product 2, 8)
(Product {getProduct = 6},17)

Prelude> (All True, (+1))<*>(All False, 0)
(All {getAll = False},1)

-}

{-
The List Applicative

(<*>) :: f (a->b) -> f a-> f b
(<*>) :: [](a->b) -> [] a-> [] b
(<*>) :: [(a -> b)] -> [a] -> [b]

With the list Applicative, we can map a plurality of functions over a plurality of values:
Prelude> [(+1), (*2)] <*> [2, 4]
[3,5,4,8]

It basically ends up being a cartesian product. Here is a more indepth example below.

(+) <$> [1, 2] <*> [3, 5]
[(+1), (+2)] <*> [3, 5]
[(3 + 1), (5 + 1), (3 + 2), (5 + 2)] -- Cartesian Product here
[4, 6, 5, 7]

It maps each function value from the first list over the second list,
applies the operations, and returns one list. The fact that it doesn’t return two lists
or a nested list or some other configuration in which both structures are preserved is the monoidal part.
The reason we don’t have a list of functions concatenated with a list of values is the
function application part.

Below you can see the relationship between lift and the functor and applicative
combinations.

Prelude> (+) <$> [1, 2] <*> [3, 5]
[4,6,5,7]
Prelude> liftA2 (+) [1, 2] [3, 5]
[4,6,5,7]
Prelude> max <$> [1, 2] <*> [1, 4]
[1,4,2,4]
Prelude> liftA2 max [1, 2] [1, 4]
[1,4,2,4]

-}

{-
Small note on lookup

Lookup is similar to get from a dictionary or hashmap. It gets a value at a certain index.
Difference is it returns a Maybe value in case nothing is there.

-}

-- Exercises 1
-- pure, (<$>), (<*>)

added :: Maybe Integer
added = pure (+ 3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

------

y' :: Maybe Integer
y' = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y' <*> z

------

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y

------
xs :: [Integer]
xs = [1, 2, 3]

ys :: [Integer]
ys = [4, 5, 6]

x2 :: Maybe Integer
x2 = lookup 3 $ zip xs ys

y2 :: Maybe Integer
y2 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x2 <*> y2

-------
{-
The Identity data type and Applicative

Using Identity, you can lift the structure up one level and apply applicative to that instead.

Prelude > xs = [1, 2, 3]
Prelude > xs' = [9, 9, 9]

Prelude > const <$> xs <*> xs'
[1, 1, 1, 2, 2, 2, 3, 3, 3]

Prelude > mkId = Identity
Prelude > const <$> mkId xs <*> mkId xs'
Identity [1, 2, 3]

-}

newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)

{-
The Constant Applicative

So this one is kinda odd. Basically the b function is normally the thing functor
and applicative would apply to. But since that is gone and functor doesn't really do anything,
you basically just take in a monoid instance for the first argument to Constant and use that.

It's like the first half of the tuple data type.

Prelude> f = Constant (Sum 1)
Prelude> g = Constant (Sum 2)

Prelude> f <*> g
Constant {getConstant = Sum {getSum = 3}

Prelude> Constant undefined <*> g
Constant (Sum {getSum =
*** Exception: Prelude.undefined

Prelude> pure 1
1

Prelude> pure 1 :: Constant String Int
Constant {getConstant = ""}

-}

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show, Ord)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant a') = Constant (a <> a')

-----------

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
    then Nothing
    else Just s

newtype Name
  = Name String
  deriving (Eq, Show)

newtype Address
  = Address String
  deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s =
  fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a =
  fmap Address $ validateLength 100 a

data Person
  = Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a

{-
I'm going to do a small write up here to go over my confusion for later incase I get confused again.

You have seen this pattern before right?

(+) <$> [1,2,3] <*> [9,9,9]

You know that works above and it's pretty much the same story now.

Person is an a -> b -> c function like plus. Specifically its a String -> String -> Person

The fmap over mkName looks like this basically ...

fmap Person (Just (Name "babe")) = Just (Person (Name "babe"))

Just (Person (Name "babe")) still needs another argument applied in order to be evaluated. Hence it is a
function wrapped in a structure which you can use in an applicative.

Just (Person (Name "babe")) <*> Just (Address "farm")

Would become Just (Person "Babe" "farm")

-}
data Cow = Cow
  { name :: String,
    age :: Int,
    weight :: Int
  }
  deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

-- Validating to get rid of empty
-- strings and negative numbers
cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  liftA3 Cow (noEmpty name') (noNegative age') (noNegative weight')

--Cow <$> (noEmpty name') <*> noNegative age' <*> noNegative weight'

{-
case noEmpty name' of
  Nothing -> Nothing
  Just nammy ->
    case noNegative age' of
      Nothing -> Nothing
      Just agey ->
        case noNegative weight' of
          Nothing -> Nothing
          Just weighty ->
            Just (Cow nammy agey weighty)
-}

-- Exercises
-- use <$> from Functor and <*> and pure from the Applicative type class to fill in missing bits of
-- the broken code below to make it work

example1 :: Maybe [Char]
example1 = const <$> Just "Hello" <*> pure "World"

example2 :: Maybe (Integer, Integer, [Char], [Integer])
example2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

{-
Applicative Laws

1. Id applied to anything outputs the exact same structure and information in it untouched.
pure id <*> v = v

pure id <*> [1 .. 5] = [1..5]
pure id <*> Just 5 = Just 5
pure id <*> Nothing = Nothing

2. Composition.
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

It is the law stating that the result of composing our functions first and
then applying them and the result of applying the functions first and then composing
them should be the same.

pure (.) <*> [(+1)] <*> [(*2)] <*> [1, 2, 3] = [(+1)] <*> ([(*2)] <*> [1, 2, 3])

pure (.) <*> Just (+1) <*> Just (*2) <*> Just 1 =  Just (+1) <*> (Just (*2) <*> Just 1)

Basically, remember that composition is g(f(x)) == f(x) * g(x). So the first half of the
equations above are just using the dot operator to pass around functions first and then apply them all afterward.
The second side is doing an operation on a value and then doing another operation afterward. The point of it
all is that they should both be equal in the end. If they are not, then it is not a valid applicative.

3. Homomorphism

A homomorphism is a structure-preserving map between two algebraic structures.
The effect of applying a function that is embedded in some structure to a value that
is embedded in some structure should be the same as applying a function to a value
without affecting any outside structure.

pure f <*> pure x = pure (f x)

pure (+1) <*> pure 1 = pure ((+1) 1)

4. Interchange

u <*> pure y = pure ($ y) <*> u

Just (+2) <*> pure 2 = pure ($ 2) <*> just (+2)

Basically, its saying that you can change around the order of each side of the apply function
and it should still be the same. On one side is the order you have seen every time, the function wrapped
in a structure on the left, and the value wrapped in a structure on the right. But you can reverse it
and it should work just fine.

Side note: ($ 2) is sorta like saying its just waiting for the function to be applied to 2. Like,
$ :: (a -> b) -> a -> b, or (a -> b) $ a -> b cause its infix, normally does the function on the left, but
by putting parens around the dollar sign a, you are changing the priorities of what needs to be done.

(Just (+2) <*> pure 2) == (pure ($ 2) <*> Just (+2))

Every Applicative instance you write should obey these four laws.
This keeps your code composable and free of unpleasant surprises.
-}