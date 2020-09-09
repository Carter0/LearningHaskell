module Notes where

import Data.Monoid

{-
A foldable data structure is some structure that
can be folded into one structure. This process relies heavily on
monoids.

class Foldable t where
    {-# MINIMAL foldMap | foldr #-}

The minimal definition here specifies that the definition must include
either foldmap or foldr. Both of those functions can be defined
in terms of eachother and both can be used to define all the other
functions in that typeclass.

The t kind signature is * -> *. So it is a higher kinded structure.
-}

{-
The Importance of Monoids

Folding necessarily implies a binary associative operation that has an
identity value.

class Foldable (t :: * -> *) where
    fold :: Monoid m => t m -> m
    foldMap :: Monoid m => (a -> m) -> t a -> m

foldmap is like fold, but it first maps over the container and makes
every element a monoid.

For fold to work the elements in it need to be a monoid.

So fold (+) [1..5] isnt going to work. The plus operator isnt a monoid.
But fold $ map Sum [1..5] will or foldMap Sum [1..5] will because
it specifies what monoid every element in the container will use.

Though is some cases, for example (++), it can identify what the
default monoid instance is.

foldMap can also have a function to map that is different than the
monoid it is using.

foldMap (*5) $ map Product [1..5]

In the above example it will do (*5) before it multiplies them all together.
5 product 10 product 15 etc before using product monoid.

-}

-- Identity Foldable Instance

data Identity a = Identity a deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

{-
You can still fold elements of only one value like Identity. What happens
is that the element inside is consumed and the resulting value is now outside
of the container.

>> foldMap (+10) (Identity 20)::Sum Int
Sum {getSum = 20}

There is no identity value after the operation.
-}

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Foldable Optional where
  foldr f z Nada = z
  foldr f z (Yep a) = f a z
  foldl f z Nada = z
  foldl f z (Yep a) = f z a

  --base case here is in the monoid
  foldMap f Nada = mempty
  foldMap f (Yep a) = f a

{-
The Foldable type class also provides a bunch of other
catamorphism functions that can be good to know. These include
null, toList, length, maximum, minimum, etc... All these functions go down the container and
output them as a single value.

The catch is for things like the tuple and either datatypes, you
can't check out half the values because the left side is part
of the datatype definition. It is the same thing as with functor.

-}

-- Implementing those function in terms of foldMap

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem toFind = getAny . foldMap (Any . (== toFind))
