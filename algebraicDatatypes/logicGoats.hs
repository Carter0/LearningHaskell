{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- pragmas don't seem to work below the module .. where lines

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

-- having the TooMany type by default requires the 
-- Generlized pragma above
newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

-- Taking in two arguments requires the FlexibileInstance
-- pragma above 
instance TooMany (Int, String) where
    tooMany (n, _) = tooMany n

instance TooMany (Int, Int) where
    tooMany (n, m) = tooMany (n + m)

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany (x) || tooMany (y)

