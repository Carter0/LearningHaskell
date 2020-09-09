module Main where

import qualified Data.Map                      as M
import           MorseCode
import           Test.QuickCheck

main :: IO ()
main = quickCheck prop_thereAndBackAgain



prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
    forAll charGen (\c -> ((charToMorse c) >>= morseToChar) == Just c)


{-
All the generators that can create a valid list of whatever.
-}
allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse
