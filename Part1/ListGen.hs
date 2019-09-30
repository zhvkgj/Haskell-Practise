module ListGen where

fibStream = 
    let fibStreamRec x y = [x] ++ fibStreamRec y (x + y)
      in fibStreamRec 0 1

fibStream' = 
    let 
        f (x : y : zs) = x : f (y : zipWith (+) (x : y : zs) (y : zs))
            in f [0, 1]


change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change n | n < 0 = []
         | otherwise = [x : y | x <- [2, 3, 7], y <- change (n - x)]