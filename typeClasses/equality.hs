module Equality where 

data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where 
    TisAn x == TisAn y = x == y

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where 
    Two a b == Two c d = 
        (a == c) && (b == d) 

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where 
    TisAnInt x == TisAnInt y = x == y
    TisAString x == TisAString y = x == y

data Pair a =
    Pair a a

-- Nope, this is right. 
instance Eq a => Eq (Pair a) where 
    Pair x y == Pair x' y' = x == x' && y == y'

data Tuple a b = 
    Tuple a b 

instance (Eq a, Eq b) => Eq (Tuple a b) where 
    Tuple a b == Tuple a' b' = a == a' && b == b'

data Which a = 
      ThisOne a 
    | ThatOne a 

-- The final line is for other things, but I don't know what. 
instance Eq a => Eq (Which a) where 
    ThisOne a == ThisOne a' = a == a' 
    ThatOne a == ThatOne a' = a == a' 
    _ == _ = False

data EitherOr a b =
      Hello a 
    | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where 
    Hello a == Hello a' = a == a' 
    Goodbye b == Goodbye b' = b == b'
    _ == _ = False
