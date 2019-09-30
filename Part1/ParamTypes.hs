module ParamTypes where
    
import Data.Char(isDigit)

data Coord a = Coord a a deriving (Show)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x2 - x1) + abs (y2 - y1)

getCenter :: Double -> Coord Int -> Coord Double
getCenter side (Coord a b) = Coord (side * (fromIntegral a + 0.5)) (side * (fromIntegral b + 0.5))  

getCell :: Double -> Coord Double -> Coord Int
getCell side (Coord a b) = Coord  (floor $ a / side) (floor $ b / side)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x : xs) | isDigit x = Just x
                   | otherwise = findDigit xs
                   
findDigitOrX :: [Char] -> Char
findDigitOrX xs = 
    case findDigit xs of
        Just x  -> x
        Nothing -> 'X'

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = [] 

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ head xs 


parsePerson :: String -> Either Error Person
parsePerson str =  