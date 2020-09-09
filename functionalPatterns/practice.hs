module Practice where

-- this is such a bad function lol. 
-- Not how it is supposed to be done.
tensDigit :: Integral a => a -> a
tensDigit x = fst $ x `divMod` 100

foldBool :: a -> a -> Bool -> a
foldBool x y bool = case bool of
    True  -> y
    False -> x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y bool | bool == True  = y
                   | bool == False = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


