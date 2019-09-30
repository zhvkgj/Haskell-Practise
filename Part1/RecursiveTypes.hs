module RecursiveTypes where

data List a = Nil | Cons a (List a) deriving (Show)
data Nat = Zero | Suc Nat           deriving (Show)
data Tree a = Leaf a | Node (Tree a) (Tree a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x (xs)) = x : fromList xs 

toList :: [a] -> List a
toList [] = Nil
toList (x : xs) = Cons x (toList xs)

fromNat :: Nat -> Integer
fromNat Zero    = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc (toNat $ n - 1)

add :: Nat -> Nat -> Nat
add x y = toNat $ fromNat x + fromNat y

mul :: Nat -> Nat -> Nat
mul x y = toNat $ fromNat x * fromNat y

fac :: Nat -> Nat
fac x = 
    let 
      facRec acc 0 = acc
      facRec acc n = (facRec $! n * acc) $! n - 1 
    in toNat $ facRec 1 $ fromNat x

height :: Tree a -> Int
height (Leaf _)          = 0
height (Node left right) = 1 + max (height left) (height right)

size :: Tree a -> Int
size (Leaf _)          = 1
size (Node left right) = 1 + size left + size right

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x)          = (1, x)
    go (Node left right) = (fst l + fst r, snd l + snd r)
      where 
        l = go left
        r = go right