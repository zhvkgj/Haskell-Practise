module Instances where 

data Color = Red | Green | Blue deriving (Read)
data LogLevel = Error | Warning | Info
data Result = Fail | Success

instance Show Color where
    show Red = "Red"
    show Blue = "Blue"
    show Green = "Green"

charToInt :: Char -> Int
charToInt x = fromEnum x - 48

stringToColor :: String -> Color
stringToColor = read

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error     = EQ
cmp Info Info       = EQ
cmp Warning Warning = EQ
cmp Info _          = LT
cmp Warning Error   = LT
cmp _ _             = GT   