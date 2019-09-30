module Folds where

import Prelude hiding (foldr, foldl1)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini []       = ini
foldr f ini (x : xs) = x `f` (foldr f ini xs) 

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f ini []       = ini
foldl' f ini (x : xs) = f ini x `seq` foldl' f (f ini x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x : xs) = foldl' f x xs
foldl1 f []       = error "List must not be empty!"

concatList :: [[a]] -> [a]
concatList = foldr (++) []
    
lengthList :: [a] -> Int
lengthList = foldr (\_ s -> 1 + s) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

meanList :: [Double] -> Double
meanList = (\p -> (fst p) / (snd p)) . foldr (\x (s, c) -> (x + s, c + 1) ) (0, 0)

evenPosOnly :: [a] -> [a]
evenPosOnly = let f (xs, r) x | r == 0 = (xs, 1)
                              | otherwise = (x : xs, 0)
                in reverse . fst . foldl' f ([], 0)

{- foldr f ([],[]) 1:2:[]
~> 1 `f` (foldr f ([],[]) 2:[])
~> 1 `f` (2 `f` (foldr f ([],[]) []))
~> 1 `f` ([2],[])
~> ([1], [2])
-}
evenPosOnly' :: [a] -> [a]
evenPosOnly' = snd . foldr (\z p -> (z : snd p, fst p)) ([], [])

lastElem :: [a] -> a
lastElem = foldl1 (\x y -> y)

{- short cut fusion 
foldr c n (build g) = g c n 
-}
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = helper (f ini) where
    helper (Just (x, y)) = x : unfoldr f y
    helper Nothing = [] 

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g (a, b) | a <= b    = Just (b, (a, p))
                 | otherwise = Nothing
                 where p = pred b