module MultiTypes where

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a
          
area :: Shape -> Double
area shape = case shape of
                 (Circle x) -> pi * (x ^ 2)
                 (Rectangle x y) -> x * y
       
isSquare :: Shape -> Bool
isSquare shape = 
    case shape of 
        (Rectangle x y) | x == y -> True
        _                        -> False

data Bit = Zero | One deriving (Eq)
data Sign = Minus | Plus deriving (Eq)
data Z = Z Sign [Bit] deriving(Eq)

intToZ :: Int -> Z
intToZ n | n < 0     = (Z Minus $ map (\x -> if x == 1 then One else Zero) $ intToZrec $ abs n)
         | n > 0     = (Z Plus $ map (\x -> if x == 1 then One else Zero) $ intToZrec $ abs n)
         | otherwise = (Z Plus [])
         where  
             intToZrec 1 = [1]
             intToZrec n = (n `mod` 2) : intToZrec (n `div` 2)

zToInt :: Z -> Int
zToInt (Z s xs) | s == Plus  = zToIntHelp xs
                | s == Minus = (-1) * zToIntHelp xs
                | otherwise  = 0
                where 
                    zToIntHelp xs = 
                        sum (zipWith (*) (map (\x -> if x == One then 1 else 0) xs) (iterate (* 2) 1))

add :: Z -> Z -> Z
add z1 z2 = intToZ $ zToInt z1 + zToInt z2
        
mul :: Z -> Z -> Z
mul z1 z2 = intToZ $ zToInt z1 * zToInt z2