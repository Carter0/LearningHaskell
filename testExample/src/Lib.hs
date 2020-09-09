module Lib
  ( runDigitConversion
  , digitToWord
  , digits
  , wordNumber
  )
where

import           Data.List                      ( intersperse
                                                , map
                                                )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )

runDigitConversion :: IO ()
runDigitConversion = do
  hSetBuffering stdout NoBuffering
  putStrLn "Enter a number with more than 1 digit: "
  string <- getLine
  let stringNum = read string :: Int
  putStrLn $ "Output is: " ++ wordNumber stringNum

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits n = go n []
 where
  go n digs | d > 0     = go d (r : digs)
            | otherwise = (r : digs)
   where
    d = n `div` 10
    r = n `mod` 10

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits

