module Monads where

import Control.Monad (liftM, ap)

data Log a = Log [String] a

instance Functor Log where
    fmap = liftM
 
instance Applicative Log where
    pure  = return
    (<*>) = ap

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = 
    (\(xs, Log ys v) -> Log (xs:ys) v) . 
    (\(Log [xs] v) -> (xs, g v)) . 
    f $ x

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log xs v) f = Log (xs ++ ys) n where Log ys n = f v

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return 