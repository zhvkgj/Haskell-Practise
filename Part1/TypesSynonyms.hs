module TypesSynonyms where

import Prelude hiding (lookup)

newtype Xor = Xor { getXor :: Bool } 
    deriving (Eq,Show)
newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Semigroup Xor where
    Xor a <> Xor b = Xor (a /= b) 
instance Monoid Xor where
    mempty = Xor False

instance Monoid a => Semigroup (Maybe' a) where
    (Maybe' Nothing) <> _ = Maybe' Nothing
    _ <> (Maybe' Nothing) = Maybe' (Nothing)
    (Maybe' (Just a)) <> (Maybe' (Just b)) = Maybe' (Just (mappend a b))
instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just (mempty))

newtype IntList = IList [Int] deriving Show

ignore :: IntList -> String
ignore (IList _) = "Hello!"

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ListMap where
    empty = ListMap []

    lookup k (ListMap []) = Nothing
    lookup k (ListMap (l:lm)) | fst l == k = Just $ snd l
                              | otherwise  = lookup k (ListMap lm)
                        
    insert k v (ListMap lm) = ListMap $ ins lm where
        ins []     = [(k,v)]
        ins (x:xs) | fst x == k = (k,v) : xs
                   | otherwise  = x : ins xs

    delete k (ListMap lm) = 
        let r = filter (\(x,y) -> x /= k) lm
        in case r of
            [] -> empty
            _  -> (ListMap r)

instance MapLike ArrowMap where
    empty = ArrowMap (\_ -> Nothing)
    lookup = flip getArrowMap
    insert k v m = ArrowMap(let f = getArrowMap m in \x -> if x /= k then f x else (Just v))
    delete k m = ArrowMap(let f = getArrowMap m in \x -> if x /= k then f x else Nothing)