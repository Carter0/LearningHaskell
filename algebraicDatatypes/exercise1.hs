module Exercise1 where

data Size =
    Size Integer deriving (Eq, Show)

data Price =
    Price Integer deriving (Eq, Show)

data Manufacturer =
    Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline =
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)


myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir (Size 200)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


-- this is bad, needs maybe monad.
getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
