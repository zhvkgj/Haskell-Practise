module OddEnum where

data Odd = Odd Integer deriving (Eq, Show)

instance Enum Odd where
    succ (Odd x) = Odd $ x + 2
    
    pred (Odd x) = Odd $ x - 2

    toEnum x = Odd . toInteger $ x
    
    fromEnum (Odd x) = fromInteger $ x

    enumFrom (Odd x) = map Odd [x, (x + 2) ..]

    enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]

    enumFromTo (Odd x) (Odd y) = map Odd [x, (x + 2) .. y]

    enumFromThenTo (Odd x1) (Odd x2) (Odd y) = map Odd [x1, x2 .. y]