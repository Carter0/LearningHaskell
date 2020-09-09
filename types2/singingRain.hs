module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString y = y ++ " over the rainbow"


sing :: Bool -> [Char]
sing bool = 
    if bool then 
        fstString x 
    else 
        sndString y 
    where 
        x = "Singin" 
        y = "Somewhere"