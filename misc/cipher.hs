module Cipher where

import           Data.Char                      ( chr
                                                , ord
                                                )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )

rotFromTo :: Int -> Int -> Int -> Int -> Int
rotFromTo f t n x = (x - f + n) `mod` (t - f) + f

rotInt :: Int -> Int -> Int
rotInt n = rotFromTo 32 126 n

rotChar :: Int -> Char -> Char
rotChar n = chr . rotInt n . ord

caesar :: Int -> String -> String
caesar n = map $ rotChar n

unCaesar :: Int -> String -> String
unCaesar = caesar . negate

cipher :: IO String
cipher = do
    hSetBuffering stdout NoBuffering
    putStr "Enter a word to encrypt: "
    word <- getLine
    let cipher = caesar 8 word
    return cipher
