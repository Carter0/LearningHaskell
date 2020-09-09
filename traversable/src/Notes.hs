module Notes where

{-
SequenceA

Basically it flips around the containers.

So basically,

>> sequenceA [Just 3, Just 4, Just 5]
Just [3, 4, 5]
-}

{-
Traversable

Traverse is fmap composed with sequenceA.
traverse f = sequenceA . fmap f

Prelude> fmap Just [1, 2, 3]
[Just 1,Just 2,Just 3]
Prelude> sequenceA $ fmap Just [1, 2, 3]
Just [1,2,3]
Prelude> sequenceA . fmap Just $ [1, 2, 3]
Just [1,2,3]
Prelude> traverse Just [1, 2, 3]
Just [1,2,3]

We’ll run through some longer examples in a moment,
but the general idea is that anytime you’re using
sequenceA . fmap f, you can use traverse to achieve the
same result in one step.

-}

{-
In a literal sense, anytime you need to flip two type constructors around,
or map something and then flip them around, that’s probably a use case for Traversable:

sequenceA :: Applicative f => t (f a) -> f (t a) flips around the containers

traverse :: Applicative f => (a -> f b) -> t a -> f (t b) maps something and then flips it around

-}

{-
Forgive the light notes. Its just this seems to simple to me now. I sorta get it just by
looking at the type signatures.
-}