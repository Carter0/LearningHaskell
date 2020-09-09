module Main where

import Control.Monad (join, liftM2)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- The Either Monad
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, First <$> arbitrary), (2, Second <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap f (First a) = First a

instance Applicative (Sum a) where
  pure b = Second b
  (<*>) (First a) _ = (First a)
  (<*>) _ (First a) = (First a)
  (<*>) (Second f) (Second b) = Second $ f b

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second b) f = f b

-- Nope
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) _ _ = NopeDotJpg

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) ident = fmap f ident

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

-- List
-- good job on this one

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil list = list
append list Nil = list
append (Cons a rest) list = Cons a $ append rest list

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (3, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f rest) list = fmap f list `append` (rest <*> list)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a as) f = f a `append` (as >>= f)

-- Method Implementations

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

main :: IO ()
main = do
  hspec $ do
    describe "Sum (Either)" $ do
      it "Functor" $ (quickBatch $ functor (undefined :: Sum String (Int, Float, Char)))
      it "Applicative" $ (quickBatch $ applicative (undefined :: Sum String (Int, Float, Char)))
      it "Monad" $ (quickBatch $ monad (undefined :: Sum String (Int, Float, Char)))
    describe "Nope" $ do
      it "Functor" $ (quickBatch $ functor (undefined :: Nope (Int, Float, Char)))
      it "Applicative" $ (quickBatch $ applicative (undefined :: Nope (Int, Float, Char)))
      it "Monad" $ (quickBatch $ monad (undefined :: Nope (Int, Float, Char)))
    describe "Identity" $ do
      it "Functor" $ (quickBatch $ functor (undefined :: Identity (Int, Float, Char)))
      it "Applicative" $ (quickBatch $ applicative (undefined :: Identity (Int, Float, Char)))
      it "Monad" $ (quickBatch $ monad (undefined :: Identity (Int, Float, Char)))
    describe "List" $ do
      it "Functor" $ (quickBatch $ functor (undefined :: List (Int, Float, Char)))
      it "Applicative" $ (quickBatch $ applicative (undefined :: List (Int, Float, Char)))
      it "Monad" $ (quickBatch $ monad (undefined :: List (Int, Float, Char)))
    describe "The J Function" $ do
      it "[[1, 2], [], [3]] is [1,2,3]" $ do
        j [[1, 2], [], [3]] `shouldBe` [1, 2, 3]
      it "Just (Just 1)) is Just 1" $ do
        j (Just (Just 1)) `shouldBe` Just 1
    describe "The l1 Function" $ do
      it "(*2) [1,2,3]" $ do
        l1 (* 2) [1, 2, 3] `shouldBe` [2, 4, 6]
      it " (*2) (Just 1) is Just 2" $ do
        l1 (* 2) (Just 1) `shouldBe` Just 2
    describe "The l2 Function" $ do
      it "Lift Over List" $ do
        l2 (*) [1, 2, 3] [1, 2, 3] `shouldBe` [1, 2, 3, 2, 4, 6, 3, 6, 9]
      it "Lift Over Maybe" $ do
        l2 (*) (Just 2) (Just 4) `shouldBe` Just 8