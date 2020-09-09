module Reverse where 

rvrse :: String
rvrse = 
    let 
        sentence = 
            "Curry is awesome"

        firstPart = 
            take 5 sentence 

        newSentence = 
            drop 9 sentence
    in 
    newSentence ++ " is " ++ firstPart

main :: IO ()
main = 
    print rvrse