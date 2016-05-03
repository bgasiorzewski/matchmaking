module Matchmaking.Common where

import Data.Int
import Data.IORef
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import System.IO.Unsafe

data Region = NA | EU
    deriving (Show, Eq, Ord, Bounded, Enum)

instance ToField Region where
    toField NA = toField (1 :: Int32)
    toField EU = toField (2 :: Int32)

type HotslogsPlayer = Int64
type HotslogsMatch = Int64

data Match = Match
    { hotslogs_match :: !HotslogsMatch
    , time_played :: !UTCTime
    , mmr_high :: !Int32
    , mmr_low :: !Int32
    , name_high :: !Text
    , name_low :: !Text
    , region :: !Region
    } deriving Show

instance ToRow Match where
    toRow (Match hm tp mh ml nh nl r) =
        [ toField hm
        , toField tp
        , toField mh
        , toField ml
        , toField nh
        , toField nl
        , toField r
        ]

data GlobalPlace = GlobalPlace { gpPlace :: !Place, gpRegion :: !Region }
    deriving (Show, Eq, Ord, Bounded)

instance Enum GlobalPlace where
    fromEnum (GlobalPlace place reg) = fromEnum place * nRegions + fromEnum reg
    toEnum n = GlobalPlace place reg
        where
        place = toEnum $ n `div` nRegions
        reg = toEnum $ n `mod` nRegions
    enumFrom gp = [gp .. maxBound]
    enumFromThen gp gp' = [gp, gp' .. maxBound]

newtype Place = Place Int32
    deriving (Show, Eq, Ord)

instance Bounded Place where
    minBound = Place 0
    maxBound = Place $ toEnum gmSize - 1

instance Enum Place where
    fromEnum (Place n) = fromEnum n
    toEnum n
        | 0 <= n && n < gmSize = Place $ toEnum n
        | otherwise = error "toEnum: argument outside of Place bounds"
    enumFrom p = [p .. maxBound]
    enumFromThen p p' = [p, p' .. maxBound]

type Grandmasters = Map GlobalPlace HotslogsPlayer

data Task
    = FetchGrandmasters !Region
    | FetchLastMatch !GlobalPlace
    | FetchMatch !Region !UTCTime !HotslogsMatch
    deriving Show

gmSize :: Int
gmSize = 100

nRegions :: Int
nRegions = 2

mmStats :: IORef (Int, Int)
mmStats = unsafePerformIO $ newIORef (0, 0)
{-# NOINLINE mmStats #-}

enumToPred :: (Eq a, Enum a, Bounded a) => a -> [a]
enumToPred a
    | a == minBound = []
    | otherwise = [minBound .. pred a]

readHTime :: String -> Maybe UTCTime
readHTime = parseTimeM True defaultTimeLocale "%-m/%-d/%Y %l:%M:%S %p"
