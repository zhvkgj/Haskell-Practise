module QSort where

import Data.Char

nTimes:: a -> Int -> [a]
nTimes elem count = nTimesRec [] count where
    nTimesRec acc 0 = acc
    nTimesRec acc k = nTimesRec (elem : acc) $! k - 1

oddsOnly :: Integral a => [a] -> [a]
oddsOnly xs = oddsOnlyRec [] xs where
    oddsOnlyRec acc [] = reverse acc
    oddsOnlyRec acc (x : xs) = 
        oddsOnlyRec (if odd x then x : acc else acc) xs

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = xs `sum2` ys `sum2` zs where
    sum2 xs [] = xs 
    sum2 [] ys = ys
    sum2 (x:xs) (y:ys) = (x+y) : sum2 xs ys

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) | null xs || x /= head xs = [x] : ys
                  | otherwise = (x : head ys) : tail ys where
                    ys = groupElems xs

groupElems' :: Eq a => [a] -> [[a]]
groupElems' [] = []
groupElems' (x : xs) = case groupElems' xs of
                        [] -> [[x]]
                        (g : gs) | x == head g -> (x : g) : gs
                                 | otherwise   -> [x] : g : gs
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    (++) (qsort $ filter (<x) xs) (x : qsort (filter (>=x) xs))

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = 
    case perms xs of
      [] -> [[x]]
      gs -> concatMap (\ts -> splits [] (x:ts)) gs where 
        splits acc [] = [] 
        splits acc (x:xs) = (x : acc ++ xs) : splits (x:acc) xs


delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words 
        
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\a b c -> a `max` b `max` c)