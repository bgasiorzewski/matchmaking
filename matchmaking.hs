module Main where

import Control.Concurrent
import Control.Monad
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
import Matchmaking.DB
import Matchmaking.Scraper

warmupTasks :: [Task]
warmupTasks =
    [ FetchGrandmasters NA
    , FetchGrandmasters EU
    ]

setUpEkg :: IO ()
setUpEkg = do
    store <- serverMetricStore <$> forkServer "0.0.0.0" 8000
    registerGauge "recent potato matches" (toEnum . fst <$> readIORef mmStats) store
    registerGauge "recent matches" (toEnum . snd <$> readIORef mmStats) store

setUpScrapers :: IO ()
setUpScrapers = do
    (mEntry, auxIdsRaw, auxDatesRaw) <- parseArgs =<< getArgs
    conn <- connectPostgreSQL "user=matchmaking dbname=matchmaking"
    entry <- maybe (loadPersist conn) (return . toEnum) mEntry
    putStrLn $ "Starting scraping from " ++ show entry
    let tasks = warmupTasks ++ map FetchLastMatch [entry ..] ++ allTasks
        allTasks = map FetchLastMatch (enumToPred entry) ++ tasks
    _ <- forkIO $ scraper conn tasks
    let auxRaw = zip (lines $! auxIdsRaw) (lines $! auxDatesRaw)
    let auxTasks = mapMaybe mkAux auxRaw
    putStrLn $ "Aux scrape queue size (assuming EU region): " ++ show (length auxTasks)
    auxConn <- connectPostgreSQL "user=matchmaking dbname=matchmaking"
    void $ forkIO $ scraper auxConn auxTasks
    where
    parseArgs [s, fIds, fDates]
        = (,,) (readMaybe s) <$> readFile fIds <*> readFile fDates
    parseArgs [s]
        = return (readMaybe s, "", "")
    parseArgs _
        = return (Nothing, "", "")
    mkAux (idRaw, dateRaw) = do
        hMatch <- readMaybe idRaw
        played <- readHTime dateRaw
        return $ FetchMatch EU played hMatch

main :: IO ()
main = do
    setUpEkg
    setUpScrapers
    hFlush stdout
    scottyOpts scottySettings $ get "/" rootApp
    where
    scottySettings = Options 0 warpSettings
    warpSettings = setHost "127.0.0.1" $ setPort 3000 defaultSettings
