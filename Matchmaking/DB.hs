module Matchmaking.DB (
    updateStats,
    matchPresent,
    insertMatch,
) where

import Database.PostgreSQL.Simple
import Data.IORef
import Control.Monad

import Matchmaking.Common

-- TODO: check for prime time
usQ :: Query
usQ = mconcat
    [ "SELECT "
    , "(SELECT COUNT(*) FROM matches"
    , " WHERE time_played > CURRENT_TIMESTAMP AT TIME ZONE 'UTC' - INTERVAL '1 day'"
    , " AND mmr_high - mmr_low > 1000), "
    , "(SELECT COUNT(*) FROM matches"
    , " WHERE time_played > CURRENT_TIMESTAMP AT TIME ZONE 'UTC' - INTERVAL '1 day')"
    ]

updateStats :: Connection -> IO ()
updateStats conn = do
    [matchCounts] <- query_ conn usQ
    writeIORef mmStats matchCounts

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
