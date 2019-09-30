module Gorka where

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool
    
class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool
    
class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a = check doesEnrageMork stomp a $ check doesEnrageGork stab a a
        where check f g a | f a = g
                          | otherwise = id