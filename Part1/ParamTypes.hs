module ParamTypes where
    
import Data.Char (isDigit)
import Data.List (lookup, isInfixOf, span)

data Coord a = Coord a a deriving (Show)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

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
parsePerson [] = Left ParsingError
parsePerson xs = builder [] [] [] $ map parse $ lines xs where
  parse :: String -> (String,String)
  parse str = if (isInfixOf " = " str) then (fst p, drop 3 $ snd p) else ([],[])
                where p = span (' ' /=) str
  builder :: String -> String -> String -> [(String,String)] -> Either Error Person
  builder fName lName age [] | null fName || null lName || null age = Left IncompleteDataError
                             | otherwise = Right $ Person fName lName (read age)
  builder fName lName age (p:ps) | (null . fst) p || (null . snd) p = Left ParsingError
                                 | fst p == "firstName" = builder (snd p) lName age ps
                                 | fst p == "lastName" = builder fName (snd p) age ps 
                                 | fst p == "age" = case (filter (False ==) $ map isDigit $ snd p) of
                                                      [] -> builder fName lName (snd p) ps
                                                      _  -> Left $ IncorrectDataError (snd p)
                                 | otherwise = builder fName lName age ps 