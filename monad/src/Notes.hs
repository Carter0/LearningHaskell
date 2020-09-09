module Notes where

{-
The Monad

Here is the type class for the monad. A monad is an applicative functor.

You can see below that a monad needs to be an applicative. An Applicative also
needs to be a functor.

So, you can write functor using monadic operations like so.
f xs = xs >>= return . f

Functor -> Applicative -> Monad
Whenever you’ve implemented an instance of Monad for a type,
you necessarily have an Applicative and a Functor, as well.
-}
import Control.Monad (join)

class Applicative m => Monad2 m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a

{-
The Three Monadic Operations

(>>=) :: m a -> (a -> m b) -> m b
(>>) :: m a -> m b -> m b
return :: a -> m a

Return is basically the same as pure. It wraps a value in some structure.

(>>) is sometimes called the sequencing operator. This operator sequences two operations
while discarding the value of the first operation.

(>>=) is called the bind operator. This is what makes a monad special.
It is also conventionally what people use when they use Monads.

To go even further, the truly unique part of monad is that is concatonating a structure
of structures into just one structure. This is unique because with this functionality
the bind operator now modifies the structure.

The book also mentions how people tend to make monads this mysterious kind of thing.
In reality, Monads are like functors and applicatives in the sense that they are
just type classes with laws.

-}
-- writing bind in terms of fmap and join
-- you can see here than monad is just an extra layer on top of what we have
-- learned so far with applicative and functor
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

{-
Do Syntax and Monads

Do syntax is really just syntactic sugar around the sequencing operator.
For example ..

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

is equivalent to

sequencing' :: IO ()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

In addition, this ...

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

is equivalent to

binding' :: IO ()
binding' =
  getLine >>= putStrLn

To go a little more indepth on the later binding' example.
getLine is a IO String value, it grabs the String inside the container
and puts it as the argument to putStrLn since putStrLn is a String -> IO () function

Why does putStrLn <$> getLine not work?

Well putStrLn is a String -> IO () function and getLine and a IO String value.
By fmappinp a String -> IO () function over a IO String value, you are getting
IO (IO ()) but thats not what we want. We want a IO () value.
So, you need a monad to join them together into that.

-}

{-
The Monad List Example

(>>=) :: Monad m => m a -> (a-> m b) -> m b
(>>=) :: [] a -> (a -> [] b) -> [] b
(>>=) :: [a] -> (a -> [b]) -> [b]

You can see it is a lot like fmap. But the arguments are flipped.
Also, its a function that outputs a list of b instead of just b.
This is because a monad will concatonate that list.

fmap :: f (a -> b) -> f a -> f b

-}

-- x <- xs bit pulls an integer out of the list
-- Also see how this outputs a new list every time but the output of the whole
-- thing is one individual list.
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

{-
The Maybe Monad

(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

Keep in mind that since the output of the function is m b, that it then
concatonates the nested Maybe b's into one.

instance Monad Maybe where
    return x = Just x
    (Just x) >>= k = k x
    Nothing >>= _ = Nothing

or

instance Monad Maybe where
    return x = Just x
    (>>=) (Just x) k = k x
    (>>=) Nothing _ = Nothing

The k value is a function that would output to a Maybe value
k x is the structure and value together. So Maybe x

So, since the k value is basically a constructor for the Maybe value this has
some implications. I like to think of it like a game of hot potato, where it passes
the value one after another to the next thing.

This also means it can never pass that value, in this case x, to something that isn't a
maybe value.
-}

data Cow = Cow
  { name :: String,
    age :: Int,
    weight :: Int
  }
  deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

-- if Cow's name is Bess,
-- it must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
   in if n == "Bess" && w > 499
        then Nothing
        else Just c

{-
If you did something like

Cow <$> noEmpty name' <*> noNegative age <*> noNegative weight
and pass that to weightCheck, you would still need to join them somehow,
which is a monadic operation.

-}
mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

{-
case noEmpty name' of
  Nothing -> Nothing
  Just nammy ->
    case noNegative age' of
      Nothing -> Nothing
      Just agey ->
        case noNegative weight' of
          Nothing -> Nothing
          Just weighty ->
            weightCheck
              (Cow nammy agey weighty)
  -}

{-
The Either Monad

(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) :: Either e a -> (a -> Either e b) -> Either e b

-}

-- The Either Examples
-- years ago
type Founded = Int

-- number of programmers
type Coders = Int

data SoftwareShop = Shop
  { founded :: Founded,
    programmers :: Coders
  }
  deriving (Eq, Show)

data FoundedError
  = NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

{-
The Monad Laws

1. The Identity Laws

-- right identity
m >>= return = m

 -- left identity
return x >>= f = fx

Both of these are saying that return should not alter the computation being done to the structure.

2. Associativity

(m >>= f) >>= g = m >>= (\x -> f x >>= g)

The left hand side is doing one function and then taking that value and then doing another.
The right hand side is doing both at the same time by wrapping them in a lambda.
Either way, they both equal eachother
-}
{-
Monad Function Composition

A reminder about composition. Its this ... g(f(x)) == g(x) and then f(x)

Remember that unlike with (.) and simple functions, with a monad you are outputting a value
wrapped in some structure. So since you are using many functions that output to a structure, you need some
of joining them together. Hence monad. Monad already kind of gives you that by default

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

Some examples

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

It does sayHi first and then readM because the function order is flipped
getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "Hello! How old are you? "
-}