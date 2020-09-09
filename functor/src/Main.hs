module Main where

import Test.QuickCheck
import Test.Hspec

functorIdentity :: (Eq (f a), Functor f) => f a -> Bool
functorIdentity f = (fmap id f) == f

functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fn f) (Fn g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

---------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where 
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where 
  arbitrary = identityGen 

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do 
  a <- arbitrary
  return $ Identity a

{-
If you want to sample this input try doing something like 

*Main> type G = Gen (Identity Int)
*Main> sample' (identityGen :: G)
[Identity 0,Identity 2,Identity (-4),Identity 5,Identity (-5),Identity 6,Identity (-6),Identity 8,Identity (-3),Identity (-4),Identity 16]
-}
------------

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where 
  fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where 
  arbitrary = pairGen 

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do 
  a <- arbitrary
  return $ Pair a a 

-----------

data Two a b = Two a b deriving (Eq, Show)

-- remember only the last one changes.
instance Functor (Two a) where 
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where 
  arbitrary = twoGen 

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do 
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

--------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where 
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
  arbitrary = threeGen 

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do 
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c



{-
FId1 is an example of a higher kinded type. It is a type that takes in another type.
So f would have the type * -> *. 
-}
type FId1 f a = f a -> Bool
type FComp1 f a b c = Fun a b -> Fun b c -> f a -> Bool

main :: IO ()
main = hspec $ do
    describe "Identity Properties for Functor" $ do
        it "Identity Identity" $ property (functorIdentity :: FId1 Identity String)
        it "Pair Identity" $ property (functorIdentity :: FId1 Pair String)
        it "Two Identity" $ property (functorIdentity :: FId1 (Two String) Int)
        it "Three Identity" $ property (functorIdentity :: FId1 (Three String Int) String)
    
    describe "Composition Property for Functor" $ do 
        it "Identity Composition" $ property (functorCompose :: FComp1 Identity String Float Int)
        it "Pair Composition" $ property (functorCompose :: FComp1 Pair String String Int)
        it "Two Composition" $ property (functorCompose :: FComp1 (Two Float) String Int Char)
        it "Three Composition" $ property (functorCompose :: FComp1 (Three Char String) Float Int Char)
