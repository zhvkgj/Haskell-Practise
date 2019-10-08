module MonadState where

import Control.Monad.State.Lazy

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

numberTree' :: Tree () -> Tree Integer
numberTree' tree = fst $ traverseTree (tree, 1) where
    traverseTree p = 
        case p of
            (Leaf (), acc)      -> (Leaf acc, acc + 1)
            (Fork l () r, acc)  -> let left  = traverseTree (l, acc)
                                       right = traverseTree (r, snd left + 1)
                                   in (Fork (fst left) (snd left) (fst right), snd right)

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (traverseTree tree) 1 where
    traverseTree tree = do
        t <- return tree
        case t of
            Leaf () -> do
                st <- get
                modify (+1)
                return (Leaf st)
            Fork l () r -> do
                l' <- traverseTree l
                st <- get
                modify (+1)
                r' <- traverseTree r
                return (Fork l' st r')