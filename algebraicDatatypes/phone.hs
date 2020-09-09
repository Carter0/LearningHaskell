module Phone where

data NumberPressed = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
data TimesPressed = OnePress | TwoPress | ThreePress

data DaPhone =
     DaPhone NumberPressed TimesPressed
    

