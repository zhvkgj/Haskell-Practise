module Records where

import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Function ((&))

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"
    
data LogLevel = Error | Warning | Info
    
data LogEntry = LogEntry { timestamp :: UTCTime
                         , logLevel :: LogLevel
                         , message :: String
                         }
    
logLevelToString :: LogLevel -> String
logLevelToString Error   = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info    = "Info"

logEntryToString :: LogEntry -> String
logEntryToString log = 
    (timeToString $ timestamp log) 
    ++ ": " 
    ++ (logLevelToString $ logLevel log) 
    ++ ": " 
    ++ (message log) 
    
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int 
                     } deriving (Show)

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 {lastName = p1 & lastName}

abbrFirstName :: Person -> Person
abbrFirstName p = 
    case p & firstName of
        []     -> p 
        [x]    -> p
        x : xs -> p {firstName = x : "."}                  