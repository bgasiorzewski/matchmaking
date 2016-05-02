module Main where

import Control.Concurrent
import Data.IORef
import Database.PostgreSQL.Simple
import System.Environment
import System.Metrics
import System.Remote.Monitoring
import Text.Read (readMaybe)
import Web.Scotty

import Matchmaking.Common
import Matchmaking.Scraper
import Matchmaking.App

warmupTasks :: [Task]
warmupTasks =
    [ FetchGrandmasters NA 0
    , FetchGrandmasters NA 1
    , FetchGrandmasters EU 0
    , FetchGrandmasters EU 1
    ]

main :: IO ()
main = do
    store <- serverMetricStore <$> forkServer "0.0.0.0" 8000
    registerGauge "potato matches" (toEnum . fst <$> readIORef mmStats) store
    registerGauge "all matches" (toEnum . snd <$> readIORef mmStats) store
    args <- getArgs
    let entry = case args of
            [s] -> maybe minBound toEnum $ readMaybe s
            _ -> minBound
    putStrLn $ "Starting scraping from " ++ show entry
    let tasks = warmupTasks ++ map FetchLastMatch [entry ..] ++ allTasks
        allTasks = map FetchLastMatch (enumToPred entry) ++ tasks
    --putStrLn $ show $ take 420 tasks
    conn <- connectPostgreSQL "user=matchmaking dbname=matchmaking"
    _ <- forkIO $ scraper conn tasks
    scotty 3000 $ get "/" $ rootApp
