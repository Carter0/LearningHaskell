{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Notes where

{-
What is a Functor?

A functor is a way of applying some function over a structure without altering
the structure. Map is an example of a functor, though it only works over lists. fmap works
over structures other than lists.

-}

-- This is the implementation of Functor.
-- Replace f with Maybe or [] or Either
--class Functor f where
--  fmap :: (a -> b) -> f a -> f b

{-
Going over that definition

class is the keyword we use, as usual, to begin the definition of a type class.
Functor is the name of the type class we are defining.

Type classes in Haskell usually refer to a type.
The letters themselves, as with type variables in type signatures,
do not mean anything special. f is a conventional letter to choose when
referring to types that have functorial structure.
The f must be the same f throughout the type class definition.

The where keyword ends the declaration of the type class name and associated types.
After the where, the operations provided by the type class are listed.

The argument f a is a Functor f that takes a type argument a.
That is, the f is a type that has an instance of the Functor type class.

The return value is f b. It is the same f from f a, while the type argument b
possibly but not necessarily refers to a different type.
-}

{-
Prelude> map (+1) (Just 1) -> errors out. map only works with lists.
Prelude> fmap (+1) (Just 1) -> Just 2, this works
Prelude> fmap (10/) (4, 5) -> (4,2.0)
-}

{-
The structure for fmap works a little differently, though similarly, depending on the
type it is being applied over.

So for a list it is (a -> b) -> [ ] a -> [ ] b
but for Maybe it is (a -> b) -> Maybe a -> Maybe b
and for Either it is (a -> b) -> E e a -> E e b

So it is similar but different depending on the type.
You can also see the pattern above.

-}

{-
The f in the type class

What we know so far ...
1. The f in the type class definition must be the same f throughout the
entire definition
2. The f must refer to a type that implements the type class

The f must also have the kind of * -> *. This means that it is awaiting application to another
type of *.

Small note on kinds: remember that a fully applied type must have the kind of *.
So, since the type definition has the type f a and f b and f is a function that takes in a type with kind of
*, then f must a function of kind * -> *. Or in other words, it takes in another fully applied type.

An interlude on Kinds

Prelude> :k (->)
(->) :: * -> * -> *

Above illustrated that the arrow function takes in a kind and outputs another kind. So,
in order for that function to be valid, it just has to take in something that outputs a kind to the arrow.

-}

class Sumthin a where
  s :: a -> a

{-
Remember (->) has * -> * arguments.

So, a must be a fully applied type. Because only a fully applied type has a * kind.
So, it is therefore * -> *.
-}

--class Else where
--  e :: b -> f (g a b c)

{-
b, like the a above it, has a * kind. So its valid in the arrow syntax.
f (g a b c) is a bit more complicated.
f is a type constructor that takes in one complete thing surrounded by parens.
So, f has a kind of * -> *.

Now g takes in 3 arguments for three different complete types.
So g has a kind of * -> * -> * -> *
a, b, c are each a kind of * and (g a b c) evaluated to a kind of *.

Since f (g a b c) eventually evaluates to a complete type of some kind *, it is valid in the
arrow syntax.
-}

class Biffy where
  slayer :: e a b -> (a -> c) -> (b -> d) -> e c d

{-
So, since the argument to (->) must be a kind. e a b must be a type constructor that evaluates to *.

(a -> c) is a function that takes in * kind and outputs a * kind. Same with (b -> d). This makes
it valid (->) syntax.

e c d, like e a b before it, must be a type constructor that evaluate to *.
-}

{-
Some Failures

class Impish v where
    impossibleKind :: v -> v a

This fails because v is defined in the type class definition and it has no type arguments.
Since the v in the class definition must be the same throughout its implementation, it can't have and then
not have a type argument.

class AlsoImp v where
    nope :: v a -> v

Same story as above.

data FixMePls =
    FixMe
    | Pls
    deriving (Eq, Show)

instance Functor FixMePls where fmap =
error
    "it doesn't matter, it won't compile"

So, keep in mind that Functor needs to take in a type constructor of * -> *. 
FixMePls has a kind of *. Doing something like ... 

data FixMePls2 a = FixMe2 a | Pls 

would make it a * -> * and therefore you could implement an instance of 
functor for that type.
-}

{-
-- <$> is the infix alias for fmap:
(<$>) :: Functor f => (a -> b) -> f a -> f b
-}

{-
The Two Functor Laws

1. Identity 
Passing in id as the function should just output the same data structure unchanged 

2. Composition 
fmap (f . g) [1..5] == (fmap f) . (fmap g) $ [1 .. 5]

Prelude> fmap ((+1) . (*2)) [1..5] 
[3,5,7,9,11]
Prelude> fmap (+1) . fmap (*2) $ [1..5] 
[3,5,7,9,11]

Both of those things need to be obeyed in order for it to be a valid functor. 

To continue, fmap should never touch the structure of the data, on the info on the inside of the 
structure. 

For example. fmap never messes with the list itself, it just messes with the values inside the list. 
-}

{-
Book goes on to talk about how you can use fmap to map through nested structures and how 
you can apply fmap in interesting ways to touch different parts of the structure.
Some examples below.

Also in the case of something like Two a b, only b would change. 

so it would be like 

instance Functor (Two a) of 
  ...
-}


-- >>> a 
-- [2]
a :: [Int]
a = fmap (+1) $ read "[1]" :: [Int]

-- >>> b
-- Just ["Hi,lol","Hellolol"]
b :: Maybe [[Char]]
b = fmap (fmap (++ "lol")) (Just ["Hi,", "Hello"])

-- >>> c 1
-- -2
c :: Integer -> Integer
c = fmap (*2) (\x -> x - 2)

-- >>> d 0
-- "1[0,1,2,3]"
d :: Integer -> [Char]
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

{-
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read ("123"++) show ioi 
    in (*3) changed
-}


functorIdentity :: (Eq (f a), Functor f) => f a -> Bool
functorIdentity f = (fmap id f) == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

data Possibly a = 
  LolNope
  | Yeppers a 
  deriving (Eq, Show)

instance Functor Possibly where 
  fmap f (Yeppers a) = Yeppers (f a)
  fmap f LolNope = LolNope


data Sum a b =
  First a 
  | Second b 
  deriving (Eq, Show)

instance Functor (Sum a) where 
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)