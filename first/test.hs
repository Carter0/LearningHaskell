sayHello :: String -> IO ()
sayHello x = 
    putStrLn ("Hello" ++ x ++ "!")

triple x = 
    x * 3

half x = 
    x / 2

square x = 
    x * x

example x =
    pi * (square x)

test1 =
    let 
        x = 3; 
        y = 1000 
    in  
        x * 3 + y

test1Where = 
    x * 3 + y 
    where 
        x = 3 
        y = 1000

test2 = 
    let 
        y = 10
        x = 10 * 5 + y 
    in 
        x * 5


test2Where = 
    x * 5
    where
        y = 10
        x = 10 * 5 + y

test3 = 
    let 
        x = 7
        y = negate x
        z = y * 10 
    in 
        z / x + y

test3Where = 
    z / x + y
    where
        x = 7
        y = negate x
        z = y * 10


waxOn = 
    x * 5
    where 
        z = 7
        y = z + 8
        x = y ^ 2

waxOff x = 
    triple x
        