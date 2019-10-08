module RecursiveTypes where

data List a = Nil | Cons a (List a) deriving (Show)
data Nat = Zero | Suc Nat           deriving (Show)
data Tree a = Leaf a | Node (Tree a) (Tree a)

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

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
{-
fac :: Nat -> Nat
fac x = 
    let 
      facRec acc 0 = acc
      facRec acc n = (facRec $! n * acc) $! n - 1 
    in toNat $ facRec 1 $ fromNat x
-}
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

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand (e1 :*: e) :+: expand (e2 :*: e)
expand (e :*: (e1 :+: e2)) = expand (e :*: e1) :+: expand (e :*: e2)
expand (e1 :+: e2)         = expand e1 :+: expand e2
expand (e1 :*: e2)         = 
  let
    lExp = expand e1
    rExp = expand e2
  in if lExp == e1 && rExp == e2 
    then lExp :*: rExp 
    else expand (lExp :*: rExp)
expand e = e