module Examples where

-- FOLDR

-- returns a true result as 
-- soon as it finds one true
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z []       = z
myFoldr f z (x : xs) = f x (myFoldr f z xs)


{-
example = foldr (+) 0 [1, 2, 3]

turns into 

(+) 1 ((+) 2 (+) 3 0)

in short. 

Or ... 
1 + (2 + (3 + 0))
1 + (2 + (3 + 0)) 
1 + (2 + 3)
1+5
6
-}

-- FOLDL
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc []       = acc
myFoldl f acc (x : xs) = myFoldl f (f acc x) xs

{-
given 

foldl (+) 0 (1 : 2 : 3 : [])

foldl associates like this 

((0   + 1)  + 2)  + 3)

Or, in other words, it associates to the left. 


It does this by adding to an accumulator in the head.



f replaces the cons constructor. z replaces empty list 
constructor.

foldr f z [1, 2, 3]
1 `f` (foldr f z [2, 3])
1 `f` (2 `f` (foldr f z [3]))
1 `f` (2 `f` (3 `f` (foldr f z []))) 1 `f` (2 `f` (3 `f` z))

The parenthesis are real. Above the folding starts at 3 f z 
and then goes from there


Therefore Associativity also matters with folding. 


foldr (^) 2 [1..3]
(1 ^ (2 ^ (3 ^ 2)))
(1 ^ (2 ^ 9)) 1 ^ 512
1
Contrast that with this:
foldl (^) 2 [1..3] ((2 ^ 1) ^ 2) ^ 3 (2 ^ 2) ^ 3
4^3
64

TLDR for carter:

It really helps here to traverse down the spine
so stop thinking of the folding function
and think of all the parens. 

foldr (+) 0 [1..3]

1 + (2 + (3 + 0))

foldr const 0 [1..3]
1 const (2 )
1

then it stops. because const forces the 
first argument. Haskell is lazy-evaluated
so it will never do more work than it needs to.

Another thing about foldl is this. Since the function you
pass operates on the list instead of the element. It's 
type signature would be like b -> a -> b.

So that would unravel to...

([] : 1) : 2) : 3)

Now for cons, which is an a -> [a] -> [a] function, we need to 
flip the arguments. Remember, the first argument to foldl is
a list. Not the element. 

So it would be foldl (flip :) [] [1, 2, 3]

foldr const 0 [1 .. 5]

1 const (2 const (3 const (4 const (5 const 0))))
1

foldr (flip const) 0 [1 .. 5]
1 (flip const) (2 (flip const) (3 (flip const) (4 (flip const) (5 (flip const) 0))))
1

-}


