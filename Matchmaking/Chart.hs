module Matchmaking.Chart where

import Data.Time
import Database.PostgreSQL.Simple.FromRow
import Data.Csv
import qualified Data.ByteString.Char8 as SC

data ChartRow = ChartRow
    { chartDate :: Day
    , chartPotato :: Int
    , chartGames :: Int
    } deriving Show

instance FromRow ChartRow where
    fromRow = ChartRow <$> field <*> field <*> field

instance ToNamedRecord ChartRow where
    toNamedRecord (ChartRow d p g) = namedRecord
        [ ("date", SC.pack $ showGregorian d)
        , ("mismatched games", SC.pack $ show p ++ "/" ++ show g)
        ]

instance DefaultOrdered ChartRow where
    headerOrder _ = header ["date", "mismatched games"]
