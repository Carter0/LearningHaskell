module Kind where

{-

As we discussed in the last chapter, these are examples of type constants:
Prelude> :kind Int
Int :: *
Prelude> :k Bool
Bool :: *
Prelude> :k Char
Char :: *



Here is an example of a type constructor.

data Example a = Blah | Woot a 

:k Example 
Example :: * -> *

You need to give it a type in order for it to make another type.


As with the effect of currying in type signatures, applying Maybe to an a 
type constructor relieves us of one arrow and gives it the kind *, or star:

Prelude> :k Maybe 
Maybe :: * -> * 

See how here it takes in the Int type.
Prelude> :k Maybe Int 
Maybe Int :: *


On the other hand, Either has to be applied to two arguments, an a and a b, 
so the kind of Either is “star to star to star”:

Prelude> :k Either 
Either :: * -> * -> *

And, again, we can query the effects of applying it to arguments:
Prelude> :k Either Int
Either Int :: * -> *

Prelude> :k Either Int String 
Either Int String :: *


-}
