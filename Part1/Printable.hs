module Test where

class Printable a where
    toString :: a -> [Char]
    toString a = []
    
instance Printable Bool where
    toString True = "true"
    toString _ = "false"
    
instance Printable () where
    toString () = "unit type"
    
instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"