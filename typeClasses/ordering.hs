module Ordering where 

-- Ord is <, > and requires Eq, which is ==
data DayOfWeek = 
    Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Ord, Show, Eq)



