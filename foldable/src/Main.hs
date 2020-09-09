module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap mf (Constant b) = mf b

data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'

data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3