module Main where

import           Test.Hspec
import           Test.QuickCheck
import           Lib
import           Data.List                      ( sort )



{-
Example 

prop_thereAndBackAgain :: Property 
prop_thereAndBackAgain =
    forAll charGen
    (\c -> ((charToMorse c)
        >>= morseToChar) == Just c) 
        
main :: IO ()
main = quickCheck prop_thereAndBackAgain

-}
-- module main needs to be here
{-

-- You can do it like this but I think the other
-- way is better

main :: IO ()
main = do
    quickCheck halfProperty
    quickCheck stringProperty
    quickCheck listProperty
    quickCheck plusAssocativeInt
    quickCheck plusCommutativeInt
    quickCheck multiplicationAssociativeInt
    hspecTests
-}

main :: IO ()
main = hspec $ do
    describe "digitToWord" $ do
        it "return zero for 0" $ do
            digitToWord 0 `shouldBe` "zero"
        it "return one for 1" $ do
            digitToWord 1 `shouldBe` "one"

    describe "digits" $ do
        it "returns [1] for 1" $ do
            digits 1 `shouldBe` [1]
        it "returns [1, 0, 0] for 100" $ do
            digits 100 `shouldBe` [1, 0, 0]

    describe "wordNumber" $ do
        it "one-zero-zero given 100" $ do
            wordNumber 100 `shouldBe` "one-zero-zero"
        it "nine-zero-zero-one for 9001" $ do
            wordNumber 9001 `shouldBe` "nine-zero-zero-one"

    describe "half" $ do
        it "half times two equals identity" $ property halfProperty

    describe "Ordered List" $ do
        it "ordered list of ints" $ property listIntProperty
        it "Ordered list of strings" $ property stringListProperty

    describe "Addition Properties" $ do
        it "Int Associativity" $ property plusAssocativeInt
        it "Int Commutativity" $ property plusCommutativeInt

    describe "Multiplication Properties" $ do
        it "Int Mult Associativity" $ property multiplicationAssociativeInt
        it "Int Mult Commutative" $ property multCommutativeInt

    describe "Mod Rem Quot Properties" $ do
        it "Quot to rem property" $ property quotToRemInt
        it "Div to mod property" $ property divToModInt

    describe "Reverse Identity" $ do
        it "Reversing a list twice gives the original list"
            $ property reverseTwiceStringIdentity

    describe "Dollar and Point Props" $ do
        it "Definition of Dollar Prop" $ property prop_dollarStringInt
        it "Definition of Point prop" $ property prop_dotStringIntString

    describe "Foldr cons, append, and concat properties" $ do
        it "Cons and Append property" $ property consAppendPropInt
        it "Append and concat property" $ property concatAppendPropInt






-------------------
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (* 2) . half

-- triple equals gives back a property
halfProperty :: Double -> Property
halfProperty int = halfIdentity int === int

-------------------
listIntProperty :: [Int] -> Bool
listIntProperty xs = listOrdered (sort xs)

stringListProperty :: [String] -> Bool
stringListProperty xs = listOrdered (sort xs)

-- should hold true for any sorted list
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_      , False) = status
    go y (       Nothing, t    ) = (Just y, t)
    go y (       Just x , t    ) = (Just y, x >= y)

---------
plusAssocativeInt :: Int -> Int -> Int -> Bool
plusAssocativeInt x y z = plusAssocative x y z

plusCommutativeInt :: Int -> Int -> Bool
plusCommutativeInt x y = plusCommutative x y

plusAssocative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssocative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

--------------
multiplicationAssociativeInt :: Int -> Int -> Int -> Bool
multiplicationAssociativeInt x y z = multiplicationAssociative x y z

multiplicationAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multiplicationAssociative x y z = z * (y * x) == (x * y) * z

multCommutativeInt :: Int -> Int -> Bool
multCommutativeInt x y = multCommutative x y

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x * y == y * x

------------------
quotToRemInt :: Int -> NonZero Int -> Bool
quotToRemInt x (NonZero y) = quotToRemProperty x y

divToModInt :: Int -> NonZero Int -> Bool
divToModInt x (NonZero y) = divToModProperty x y

quotToRemProperty :: Integral a => a -> a -> Bool
quotToRemProperty x y = (quot x y) * y + (rem x y) == x

divToModProperty :: Integral a => a -> a -> Bool
divToModProperty x y = (div x y) * y + (mod x y) == x

---------

reverseTwiceStringIdentity :: [String] -> Bool
reverseTwiceStringIdentity list = reverseIdentity list

reverseIdentity :: Eq a => [a] -> Bool
reverseIdentity list = reverseTwice list == list

reverseTwice :: [a] -> [a]
reverseTwice = reverse . reverse

------------


dollar :: t1 -> (t1 -> t2) -> t2
dollar a f = f $ a

noDollar :: (t1 -> t2) -> t1 -> t2
noDollar f a = f a

dollarProp :: Eq t2 => (t1 -> t2) -> t1 -> Bool
dollarProp f a = dollar a f == noDollar f a

-- this is odd syntax you haven't seen before.
-- without that syntax it doesn't work. 
prop_dollarStringInt :: Fun String Int -> String -> Bool
prop_dollarStringInt (Fn f) s = dollarProp f s

point :: (b -> c) -> (a -> b) -> a -> c
point f g = f . g

noPoint :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
noPoint f g x = f (g x)

pointProp :: Eq t2 => (b -> t2) -> (a -> b) -> a -> Bool
pointProp f g x = point f g x == noPoint f g x

prop_dotStringIntString :: Fun Int String -> Fun String Int -> String -> Bool
prop_dotStringIntString (Fn f) (Fn g) s = pointProp f g s

------------


consAppendProp :: Eq a => [a] -> [a] -> Bool
consAppendProp base xs = foldr (:) base xs == xs ++ base

consAppendPropInt :: [Int] -> [Int] -> Bool
consAppendPropInt start end = consAppendProp start end

concatAppendProp :: (Eq a, Foldable t) => t [a] -> Bool
concatAppendProp xs = foldr (++) [] xs == concat xs

concatAppendPropInt :: [[Int]] -> Bool
concatAppendPropInt listOfLists = concatAppendProp listOfLists

---------------

data Fool =
    Fulse
    | Frue
    deriving (Eq, Show)


foolGen :: Gen Fool
foolGen = oneof [return Frue, return Fulse]

instance Arbitrary Fool where
    arbitrary = foolGen

genFool2 :: Gen Fool
genFool2 = frequency [(1, return Fulse), (2, return Frue)]

