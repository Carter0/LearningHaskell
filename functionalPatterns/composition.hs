module Composition where

example1 :: (Num c, Foldable t) => t c -> c
example1 xs = negate . sum $ xs


{-

function composition can be understood like this. 

. is an infix operator where (f . g) equiv. to (\x -> f (g x))

So, explaining the above example. Function composition is where, given
some free variable x, you first apply g, then apply f to the arguments. 

(.) :: (b -> c) -> (a -> b) -> a -> c

First apply the (a -> b) function, Then the (b -> c) function, to the given a value. 

So, in the above example, you first apply sum to xs, then apply negate. 
Sum is the g in (f . g) and negate is the f. 

Why the dollar sign in the above value? Because without it the order of precidence for what 
function goes first would be wrong. A function has a 10/10 on the operator precidence scale,
so it would turn out negate . 15 without the dollar sign, which would crash. Remember,
(f . g) is the whole operation and it requires two functions.
-}
