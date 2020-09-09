module Database where

import           Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbNumber 4
    , DbString "Hello World"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr concatDates []
  where
    concatDates (DbDate time) ts = time : ts
    concatDates _             ts = ts


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr concatNums []
  where
    concatNums (DbNumber num) nums = num : nums
    concatNums _              nums = nums

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr maxDate (UTCTime (ModifiedJulianDay 0) 0)
  where
    maxDate (DbDate time) ts = max time ts
    maxDate _             ts = ts

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sumNums 0
  where
    sumNums (DbNumber num) nums = num + nums
    sumNums _              nums = nums


-- okay this one was all me tho
avgDb :: [DatabaseItem] -> Double
avgDb temp =
    fromIntegral (sumDb temp) / fromIntegral (length (filterDbNumber temp))
