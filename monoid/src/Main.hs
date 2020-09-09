module Main where

import           Test.QuickCheck
import           Test.Hspec
import           Data.Monoid

main :: IO ()
main = hspec $ do
  describe "Associative Property Test" $ do
    it "Semigroup of Trivial" $ property (semigroupAssoc :: TrivAssoc)
    it "Semigroup of Identity" $ property (semigroupAssoc :: IdentAssoc String)
    it "Semigroup of Two"
      $ property (semigroupAssoc :: TwoAssoc String (Product Int))
    it "Semigroup of Three"
      $ property (semigroupAssoc :: ThreeAssoc String (Product Int) (Sum Int))
    it "BoolConj Test" $ property (semigroupAssoc :: BoolConjAssoc)
    it "BoolDisj Test" $ property (semigroupAssoc :: BoolDisjAssoc)
    it "Or Test" $ property (semigroupAssoc :: OrAssoc String (Sum Int))
    it "Combination Test"
      $ property (combMonoidAssoc :: CombAssoc Int (Sum Int))
  describe "Left and Right Identity Property Tests" $ do
    it "Left Identity of Trivial" $ property (monoidLeftIdentity :: TrivIdent)
    it "Right Identity of Trivial" $ property (monoidRightIdentity :: TrivIdent)
    it "Left Identity of Identity"
      $ property (monoidLeftIdentity :: IdentIdent String)
    it "Right Identity of Identity"
      $ property (monoidRightIdentity :: IdentIdent String)
    it "Two Identity"
      $ property (monoidLeftIdentity :: TwoIdent (String) (Sum Int))
    it "Three Identity" $ property
      (monoidLeftIdentity :: ThreeIdent (String) (Sum Int) (Product Int))
    it "BoolConj Identity" $ property (monoidRightIdentity :: BoolConjIdent)
    it "BoolDisj Identity" $ property (monoidRightIdentity :: BoolDisjIdent)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

-- this one won't work with the rest because you need to input a value.
combMonoidAssoc
  :: (Eq b, Monoid b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combMonoidAssoc f g h a =
  unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

{-
When you set mempty in each instance, you are replacing the mempty value here
so for identity a it would be identity mempty so if a = Identity "Ban"

monoidLeftIdentity Identity "Ban" =
  (Identity mempty <> Identity "Ban") == Identity "Ban"

which then goes to 

mempty <> "Ban" and in the case of strings mempty is ""
so its "" <> "Ban"

so its "Ban"
-}
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
-----------------
data Trivial' = Trivial' deriving (Eq, Show)

instance Semigroup Trivial' where
  _ <> _ = Trivial'

instance Monoid Trivial' where
  mempty  = Trivial'
  mappend = (<>)

instance Arbitrary Trivial' where
  arbitrary = return Trivial'

type TrivAssoc = Trivial' -> Trivial' -> Trivial' -> Bool
type TrivIdent = Trivial' -> Bool
-----------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity a1 = Identity (a <> a1)

{- 
Don't forget two things ..
1. You are making an instance of monoid for Identity of a, not a. So 
you are not going to have a value like [] or 0 or something. 
2. a is a monoid. So it is either a mempty or something else monoidic. 
In this case it makes sense that its mempty because when in semigroup you 
combine values the mempty would be used there.
-}
instance Monoid a => Monoid (Identity a) where
  mempty  = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

type IdentAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool
type IdentIdent a = Identity a -> Bool
----------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a1 b1 <> Two a2 b2 = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty  = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool
type TwoIdent a b = Two a b -> Bool

------------

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty  = Three mempty mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c ) where
  arbitrary = threeGen

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool
type ThreeIdent a b c = Three a b c -> Bool

-----------

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj _    <> _             = BoolConj False

instance Monoid BoolConj where
  mempty  = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolConjIdent = BoolConj -> Bool

----------

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True <> _             = BoolDisj True
  _             <> BoolDisj True = BoolDisj True
  _             <> _             = BoolDisj False

instance Monoid BoolDisj where
  mempty  = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolDisjIdent = BoolDisj -> Bool
------------

{-
Can't print this out, not sure why. 
Perhaps have to implement show?
-}

data Or a b =
  Fst a
  | Snd b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  Snd b <> Snd b' = Snd b
  Snd b <> _      = Snd b
  _     <> Snd b  = Snd b
  Fst a <> Fst a' = Fst a'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Fst a, return $ Snd b]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

----------

-- this one i copied because it really confused me :(
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show (Combine _) = "Combine"

{-
Here is my attempt at explaining this. 
The combine type constructor takes in a function that changes an a to a b.
So, if b has a semigroup instance, you can call the function on a to get a b,
then use the associative property to combine that b with another b.
-}
instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (\a -> f a <> g a)

-- idk lol
instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (const mempty)

-- whats CoArbitrary?
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

type CombAssoc a b = Combine a b -> Combine a b -> Combine a b -> a -> Bool
