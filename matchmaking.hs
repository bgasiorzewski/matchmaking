module Main where

import Control.Concurrent
import Data.IORef
import Data.Maybe
import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp
import System.Environment
import System.IO
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
    registerGauge "recent potato matches" (toEnum . fst <$> readIORef mmStats) store
    registerGauge "recent matches" (toEnum . snd <$> readIORef mmStats) store
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
    let auxTasks = mapMaybe mkAux auxRaw
    putStrLn $ "Aux scrape queue size (assuming EU region): " ++ show (length auxTasks)
    auxConn <- connectPostgreSQL "user=matchmaking dbname=matchmaking"
    _ <- forkIO $ scraper auxConn auxTasks
    -- flush
    hFlush stdout
    -- set up scotty
    scottyOpts scottySettings $ get "/" rootApp
    where
    scottySettings = Options 0 warpSettings
    warpSettings = setHost "127.0.0.1" $ setPort 3000 $ defaultSettings
    fallbackyEntry s = maybe minBound toEnum $ readMaybe s
    mkAux (idRaw, dateRaw) = do
        hMatch <- readMaybe idRaw
        played <- readHTime dateRaw
        return $ FetchMatch EU played hMatch
