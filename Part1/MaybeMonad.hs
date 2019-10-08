module MaybeMonad where

import Data.Char (isDigit)
import Data.Traversable (sequence)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken xs | xs == "+" = Just Plus
           | xs == "-" = Just Minus
           | xs == "(" = Just LeftBrace
           | xs == ")" = Just RightBrace
           | otherwise = case reads xs of
                             [(i, "")] -> Just $ Number i
                             _         -> Nothing

tokenize :: String -> Maybe [Token]
tokenize = traverse asToken . words 

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x | x <= 0    = []
                    | otherwise = do
                        c <- [1..x]
                        b <- [1..c]
                        a <- [1..b]
                        if (a < b && b < c && a^2 + b^2 == c^2) 
                            then return (a,b,c) 
                            else []
                        