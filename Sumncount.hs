module Sum'n'count where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0 = (0, 1)
              | otherwise = sum'n'countRec 0 0 (abs x) where
                  sum'n'countRec sum count 0 = (sum, count)
                  sum'n'countRec sum count x = 
                    sum'n'countRec (sum + n) (count + 1) m where
                        m = div x 10 
                        n = mod x 10              