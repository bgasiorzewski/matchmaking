module Matchmaking.DB (
    connectPG,
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
import System.IO.Unsafe

import Matchmaking.Chart
import Matchmaking.Common

connector :: IO Connection
connector = connectPostgreSQL "user=matchmaking dbname=matchmaking"

connRef :: IORef Connection
connRef = unsafePerformIO $ newIORef =<< connector
{-# NOINLINE connRef #-}

connectPG :: IO ()
connectPG = do
    close =<< readIORef connRef
    writeIORef connRef =<< connector

usQ :: Query
usQ = mconcat
    [ "SELECT "
    , "(SELECT COUNT(*) FROM matches"
    , " WHERE time_played > CURRENT_TIMESTAMP AT TIME ZONE 'UTC' - INTERVAL '1 day'"
    , " AND mmr_high - mmr_low > 1000 "
    , " AND time_added - time_played < '3 days'), "
    , "(SELECT COUNT(*) FROM matches"
    , " WHERE time_played > CURRENT_TIMESTAMP AT TIME ZONE 'UTC' - INTERVAL '1 day' "
    , " AND time_added - time_played < '3 days')"
    ]

cdQ :: Query
cdQ = mconcat
    [ "SELECT "
    , "CAST(time_played AS DATE) AS date_played,"
    , "COUNT(CASE WHEN mmr_high - mmr_low > 1000 THEN 1 END) AS n_potato,"
    , "COUNT(*) AS n_all "
    , "FROM matches "
    , "WHERE time_played >= '2016-05-06' "
    , "AND time_added - time_played < '3 days' "
    , "GROUP BY date_played "
    , "ORDER BY date_played ASC"
    ]

updateStats :: IO ()
updateStats = do
    conn <- readIORef connRef
    [matchCounts] <- query_ conn usQ
    writeIORef mmStats matchCounts
    cd <- query_ conn cdQ
    writeIORef chartCsv $ enc $ safeInit (cd :: [ChartRow])
    where
    -- dygraphs treats quotes as part of data
    enc = encodeDefaultOrderedByNameWith $ defaultEncodeOptions { encQuoting = QuoteNone }
    -- current day is highly volatile, hide it
    safeInit [] = []
    safeInit xs = init xs

mpQ :: Query
mpQ = "SELECT EXISTS (SELECT 1 FROM matches WHERE hotslogs_match = ?)"

matchPresent :: HotslogsMatch -> IO Bool
matchPresent hMatch = do
    conn <- readIORef connRef
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

insertMatch :: Match -> IO ()
insertMatch match = do
    conn <- readIORef connRef
    void $ execute conn imQ match

lpQ :: Query
lpQ = "SELECT next_gp FROM persist"

loadPersist :: IO GlobalPlace
loadPersist = do
    conn <- readIORef connRef
    [Only next_gp] <- query_ conn lpQ
    return $ toEnum next_gp

spQ :: Query
spQ = "UPDATE persist SET next_gp = ?"

savePersist :: GlobalPlace -> IO ()
savePersist gp = do
    conn <- readIORef connRef
    void $ execute conn spQ $ Only $ fromEnum gp
