module SeqA where

seqA :: Integer -> Integer
seqA n | n < 3 = error "Argument must be more than 2" 
       | otherwise = let 
           seqARec acc prev preprev 3 = 
             acc + prev - 2 * preprev
           seqARec acc prev preprev n = 
             seqARec (acc + prev - 2 * preprev) acc prev (n - 1)
         in seqARec 3 2 1 n