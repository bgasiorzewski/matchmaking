module Main where

import Control.Concurrent
import Data.IORef
import Data.Maybe
import Database.PostgreSQL.Simple
import System.Environment
import System.Metrics
import System.Remote.Monitoring
import Text.Read (readMaybe)
import Web.Scotty

import Matchmaking.App
import Matchmaking.Common
import Matchmaking.Scraper

warmupTasks :: [Task]
warmupTasks =
    [ FetchGrandmasters NA
    , FetchGrandmasters EU
    ]

main :: IO ()
main = do
    -- set up ekg
    store <- serverMetricStore <$> forkServer "0.0.0.0" 8000
    registerGauge "potato matches" (toEnum . fst <$> readIORef mmStats) store
    registerGauge "all matches" (toEnum . snd <$> readIORef mmStats) store
    -- set up scrapers
    args <- getArgs
    (entry, auxIdsRaw, auxDatesRaw) <- case args of
        [s, fIds, fDates] -> (,,) (fallbackyEntry s) <$> readFile fIds <*> readFile fDates
        [s] -> return (fallbackyEntry s, "", "")
        _ -> return (minBound, "", "")
    putStrLn $ "Starting scraping from " ++ show entry
    let tasks = warmupTasks ++ map FetchLastMatch [entry ..] ++ allTasks
        allTasks = map FetchLastMatch (enumToPred entry) ++ tasks
    conn <- connectPostgreSQL "user=matchmaking dbname=matchmaking"
    _ <- forkIO $ scraper conn tasks
    let auxRaw = zip (lines $! auxIdsRaw) (lines $! auxDatesRaw)
    let auxTasks = catMaybes $ map mkAux auxRaw
    putStrLn $ "Aux scrape queue size (assuming EU region): " ++ show (length auxTasks)
    auxConn <- connectPostgreSQL "user=matchmaking dbname=matchmaking"
    _ <- forkIO $ scraper auxConn auxTasks
    -- set up scotty
    scotty 3000 $ get "/" $ rootApp
    where
    fallbackyEntry s = maybe minBound toEnum $ readMaybe s
    mkAux (idRaw, dateRaw) = do
        hMatch <- readMaybe idRaw
        played <- readHTime dateRaw
        return $ FetchMatch EU played hMatch
