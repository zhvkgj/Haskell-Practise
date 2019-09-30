module Test where

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n == (-1) = 1
            | n > 1 = fiborec 0 1 n
            | otherwise = fiboruc 0 1 (-n)
    
fiborec prev acc 2 = acc + prev
fiborec prev acc n = fiborec acc (acc + prev) (n - 1)

fiboruc prev acc 2 = prev - acc
fiboruc prev acc n = fiboruc acc (prev - acc) (n - 1)