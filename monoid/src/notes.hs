module Notes where

{-
A monoid is a binary associative operation with an identity.

This means that it is a function that takes two arguments, obeys the 
associative property, and has an identity argument. An identity argument means
that there will be some value that, when combined with any other value,
always gives that other value.

Ex of identity arg. 

mappend [1..5] [] = [1..5]
mappend [] [1..5] = [1..5]


A monoid is implemented in haskell with a type class. Some common
monoids are addition, multiplication, and list concatenation.

The definition of a monoid ...

class Semigroup m => Monoid m where 
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

mappend -> how any two values that inhabit your type can be joined.
mempty -> the identity value for the mappend operation.

A type can only have one instance of monoid. This is why integer
is not a monoid; both multiplication and addition are monoidic 
operations, so which one do you use?

You can resolve this conflict by using the sum and product newtypes.

Prelude> mappend (Sum 1) (Sum 5)
Sum {getSum = 6}

Prelude> mappend (Product 5) (Product 5)
Product {getProduct = 25}

We use newtype for this because it singles out the intent. 
Newtype can not be a sum or product type, this signals to the 
programmer that the sum and product types are really just
wrappers around the data. Also, since it is only one type, it prevents
runtime overhead.

Note : (<>) is the infix operator for mappend.

Was having trouble importing the Sum and Product types, were they something
I was supposed to have made? 

Also, one cool thing about having a monoid is that you can use that function in a 
folding operation

[1, 2, 3] (+) 0 -> (+) is the monoid here.
So if you make your own monoids then you know you can fold over them.


LAWS

So, the book goes off on a slight tangent about the importance of 
algebraic laws in software engineering. It makes the argument that 
programs need to obey laws because otherwise how would we trust the program 
to work reliably? We should trust our computer programs like we trust the
fundemental principles of math, which is something I agree with.

To do this in haskell, we implement these algebraic laws as typeclasses. 

Here are the laws a monoid needs to abide.

 -- left identity
mappend mempty x = x

-- right identity
mappend x mempty = x

-- associativity
mappend x (mappend y z) = mappend (mappend x y) z

mconcat = foldr mappend mempty


Also worth noting that not every monoid fits so neatly into the + and * and concat 
operations. The book says some are a bit more complicated to pin down. 

"Appending is perhaps best thought of not as a way of combining values 
in the way that addition or list concatenation does, but as a way 
to condense any set of values to a summary value." 


So, for Booleans you have Any and All. 

All returns true if all the values are a certain thing. 

Prelude> All True <> All True
All {getAll = True}

Prelude> All True <> All False
All {getAll = False}


Any returns true if any of the values are something

Prelude> Any True <> Any False
Any {getAny = True}

Prelude> Any False <> Any False
Any {getAny = False}

The Maybe Value has two monoids as well, First and Last. 

First returns the first value.

Prelude> x = First (Just 1) Prelude> 
y = First (Just 2) 
Prelude> x `mappend` y 
First {getFirst = Just 1}

And last returns the last monoid

Prelude> x = Last (Just 1) 
Prelude> y = Last (Just 2) 
Prelude> x `mappend` y 
Last {getLast = Just 2}

Both will return something Just if there is a Nothing value. 
-}

import           Data.Monoid

-- import           Test.QuickCheck

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)


instance Monoid a => Semigroup (Optional a) where
    (<>) Nada      (Only a)  = (Only a)
    (<>) (Only a ) Nada      = (Only a)
    (<>) (Only a1) (Only a2) = Only (a1 <> a2)

-- do you need mconcat here too?
instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend arg1 arg2 = arg1 <> arg2

{-
Associativity 
(a + b) + c = a + (b + c)

Commutativity
a + b = b + a
-}


type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
    e
        <> "! he said "
        <> adv
        <> " as he jumped into his car "
        <> noun
        <> " and drove off with his "
        <> adj
        <> " wife."


madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat
    [ e
    , "! he said "
    , adv
    , " as he jumped into his car "
    , noun
    , " and drove off with his "
    , adj
    , " wife."
    ]

---------

-- You can use this to test the associativity property of monoids with
-- quickcheck
asc :: (Eq a, Monoid a) => (a -> a -> a) -> a -> a -> a -> Bool 
asc (<>) a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a

{-
What is a Semigroup?

class Semigroup a where 
    (<>) :: a -> a -> a

this has one law and it is the associative property. No identity properties here.
(a <> b) <> c = a <> (b <> c)


NonEmpty: a semigroup but not a monoid. It is a list that can 
never be empty.

data NonEmpty a = a :| [a]

Here :| is an infix data constructor that takes two (type) arguments. 
It’s a product of a and [a]. It guarantees that we always have at least 
one value of type a, which [a] does not guarantee, as any list might be empty.

NonEmpty has no identity by design. The empty list is 
deliberatly excluded. 


Anything that is a monoid is by definition also a semigroup. A monoid 
has the associative property a semigroup has plus the identity value. Therefore,
a monoid is stronger than a semigroup.

class Semigroup a => Monoid a where
    ...

An interesting note the chapter ends on is the inverse relationship between
how specific a datatype is and how many operations you can perform using that datatype. 

So, if you take in an generic a, then you can't do much with it because
no one knows what it is. But if you take in a [a] or a Num, or something, you now 
have more specific operations and things you can do with that data.
-}