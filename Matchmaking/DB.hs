module Matchmaking.DB (
    updateStats,
    matchPresent,
    insertMatch,
    loadPersist,
    savePersist,
) where

import Database.PostgreSQL.Simple
import Data.Csv hiding (Only)
import Data.IORef
import Control.Monad

import Matchmaking.Chart
import Matchmaking.Common

usQ :: Query
usQ = mconcat
    [ "SELECT "
    , "(SELECT COUNT(*) FROM matches"
    , " WHERE time_played > CURRENT_TIMESTAMP AT TIME ZONE 'UTC' - INTERVAL '1 day'"
    , " AND mmr_high - mmr_low > 1000), "
    , "(SELECT COUNT(*) FROM matches"
    , " WHERE time_played > CURRENT_TIMESTAMP AT TIME ZONE 'UTC' - INTERVAL '1 day')"
    ]

cdQ :: Query
cdQ = mconcat
    [ "SELECT "
    , "CAST(time_played AS DATE) AS date_played,"
    , "COUNT(CASE WHEN mmr_high - mmr_low > 1000 THEN 1 END) AS n_potato,"
    , "COUNT(*) AS n_all "
    , "FROM matches "
    , "WHERE time_added >= '2016-05-06' "
    , "AND time_played >= '2016-05-06' "
    , "GROUP BY date_played "
    ]

updateStats :: Connection -> IO ()
updateStats conn = do
    [matchCounts] <- query_ conn usQ
    writeIORef mmStats matchCounts
    cd <- query_ conn cdQ
    writeIORef chartCsv $ encodeDefaultOrderedByName (cd :: [ChartRow])

mpQ :: Query
mpQ = "SELECT EXISTS (SELECT 1 FROM matches WHERE hotslogs_match = ?)"

matchPresent :: Connection -> HotslogsMatch -> IO Bool
matchPresent conn hMatch = do
    [Only present] <- query conn mpQ $ Only hMatch
    return present

imQ :: Query
imQ = mconcat
    [ "INSERT INTO matches"
    , "( hotslogs_match"
    , ", time_played"
    , ", mmr_high"
    , ", mmr_low"
    , ", name_high"
    , ", name_low"
    , ", hotslogs_region"
    , ") VALUES (?,?,?,?,?,?,?)"
    ]

insertMatch :: Connection -> Match -> IO ()
insertMatch conn match = void $ execute conn imQ match

lpQ :: Query
lpQ = "SELECT next_gp FROM persist"

loadPersist :: Connection -> IO GlobalPlace
loadPersist conn = do
    [Only next_gp] <- query_ conn lpQ
    return $ toEnum next_gp

spQ :: Query
spQ = "UPDATE persist SET next_gp = ?"

savePersist :: Connection -> GlobalPlace -> IO ()
savePersist conn gp = void $ execute conn spQ $ Only $ fromEnum gp
