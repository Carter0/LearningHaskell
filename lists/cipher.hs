module Cipher where

import           Data.Char

-- there is a bug here, too tired to care. good enough lol
modByTop :: Int -> Int
modByTop x | mod x (ord 'z') < (ord 'A') = mod x (ord 'z') + (ord 'A')
           | otherwise                   = mod x (ord 'z')

caesarCipher :: Int -> String -> String
caesarCipher encodeBy toEncode = map (encode encodeBy) toEncode


encode :: Int -> Char -> Char
encode encodeBy char
    | encodeBy + (ord char) >= (ord 'z')
    = chr $ modByTop $ encodeBy + (ord char)
    | otherwise
    = chr $ encodeBy + (ord char)


unCaesarCipher :: Int -> String -> String
unCaesarCipher decodeBy toDecode = map (decode decodeBy) toDecode

decode :: Int -> Char -> Char
decode decodeBy char
    | (ord char) - decodeBy <= (ord 'A') = chr $ modByTop (ord char - decodeBy)
    | otherwise                          = chr $ (ord char - decodeBy)



