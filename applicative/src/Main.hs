{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Applicative
import Data.Monoid
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (2, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) as = fmap f as `append` (fs <*> as)

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs in take 3000 l
      ys' = let (ZipList' l) = ys in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

-- Had my own implementation, but it didn't have the right behaviour
-- fs <*> xs wont work because then it goes to the normal list applicative
-- (ZipList' fs <*> ZipList' xs) won't work alone because (:) expects a normal list
instance Applicative ZipList' where
  pure x = ZipList' [x]
  (<*>) (ZipList' []) _ = ZipList' []
  (<*>) _ (ZipList' []) = ZipList' []
  (<*>) (ZipList' (f : fs)) (ZipList' (x : xs)) =
    ZipList' $ (f x) : bs
    where
      (ZipList' bs) = (ZipList' fs <*> ZipList' xs)

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = frequency [(1, Main.Failure <$> arbitrary), (2, Main.Success <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance Functor (Validation e) where
  fmap f (Main.Failure e) = Main.Failure e
  fmap f (Main.Success a) = Main.Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure a = Main.Success a
  Main.Failure e <*> Main.Failure e' = Main.Failure $ e <> e'
  Main.Failure e <*> _ = Main.Failure e
  _ <*> Main.Failure e = Main.Failure e
  Main.Success f <*> Main.Success a' =
    Main.Success $ f a'

data Pair a = Pair a a deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  Pair fx fy <*> Pair a b = Pair (fx a) (fy b)

data Two a b = Two a b deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two m g) (Two m' b) = (Two (m <> m') (g b))

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' m b b1) (Three' m' a a1) = Three' (m <> m') (b a) (b1 a1)

main :: IO ()
main = hspec $ do
  describe "List" do
    it "Functor" $
      property (quickBatch $ functor (undefined :: List (Int, Float, String)))
    it "Applicative" $
      property
        (quickBatch $ applicative (undefined :: List (Int, Float, String)))
  describe "ZipList" do
    it "Functor" $
      property (quickBatch $ functor (undefined :: ZipList' (Int, Float, Int)))
    it "Applicative" do
      property (quickBatch $ applicative (undefined :: (ZipList' (Float, String, Char))))
  describe "Validation" do
    it "Functor" $ do
      property (quickBatch $ functor (undefined :: (Validation String (String, Int, Int))))
    it "Applicative" $ do
      property (quickBatch $ applicative (undefined :: (Validation String (String, Char, Float))))
  describe "Pair" do
    it "Functor" $ do
      property (quickBatch $ functor (undefined :: (Pair (String, Int, Char))))
    it "Applicative" $ do
      property (quickBatch $ applicative (undefined :: (Pair (String, Int, Char))))
  describe "Two" do
    it "Functor" $ do
      property (quickBatch $ functor (undefined :: (Two Char (String, Int, Char))))
    it "Applicative" $ do
      property (quickBatch $ applicative (undefined :: (Two (Sum Int) (String, Int, Char))))
  describe "Three'" do
    it "Functor" $ do
      property (quickBatch $ functor (undefined :: (Three' Char (String, Int, Char))))
    it "Applicative" $ do
      property (quickBatch $ applicative (undefined :: (Three' (Sum Int) (String, Int, Char))))

{-
data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency
      [ (1, return Fools),
        (1, return Twoo)
      ]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where
  (=-=) = eq

quickBatch (monoid Twoo)
-}