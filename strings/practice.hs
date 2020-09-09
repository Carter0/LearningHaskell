module Practice where

getPlacement :: String -> Int -> Char
getPlacement string place =  
    string !! place




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
        


