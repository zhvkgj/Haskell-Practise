module Functor where

data Point3D a = Point3D a a a deriving Show
data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor Point3D where
    fmap f (Point3D a b c) = Point3D (f a) (f b) (f c)

instance Functor GeomPrimitive where
    fmap f (Point a) = Point (fmap f a)
    fmap f (LineSegment a b) = LineSegment (fmap f a) (fmap f b)

instance Functor Tree where
    fmap f (Leaf a) = Leaf $ f <$> a
    fmap f (Branch a b c) = Branch (f <$> a) (f <$> b) (f <$> c)

instance Functor (Entry k1 k2) where
    fmap f (Entry p v) = Entry p (f v) 

instance Functor (Map k1 k2) where
    fmap f (Map xs) = Map $ map (f <$>) xs