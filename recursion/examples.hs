module Examples where

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial x - 1



-- Pattern matching using recursion. 
{-
Interesting thing here is that only argument we care about 
is the argument that changes.

You do have all the arguments, but the specific one you care about
changes.
-}
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0     n = n
incTimes times n = 1 + (incTimes (times - 1) n)


fibonnaci :: Integer -> Integer
fibonnaci 0 = 0
fibonnaci 1 = 1
fibonnaci x = fibonnaci (x - 1) + fibonnaci (x - 2)


sum2 :: Integer -> Integer
sum2 0 = 0
sum2 x = x + sum2 (x - 1)

integralMulti :: (Integral a) => a -> a -> a
integralMulti x 0     = 0
integralMulti x count = x + (integralMulti x (count - 1))

mcarthyFunction :: Integer -> Integer
mcarthyFunction n | n > 100  = n - 10
                  | n <= 100 = (mcarthyFunction (mcarthyFunction $ n + 11))



