module Integration where

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let 
  integRec sum curP 1000 = step * (sum + (f a + f b) / 2)
  integRec sum curP k = integRec (sum + f curP) (curP + step) (k + 1)
    in integRec 0 (a + step) 1 
      where step = (b - a) / 1000