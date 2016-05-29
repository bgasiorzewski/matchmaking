module Main where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.List
import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp
import System.Environment
import System.Exit
import System.IO
import System.Metrics
import System.Remote.Monitoring
import Web.Scotty
import qualified Data.ByteString.Lazy as L

import Matchmaking.App
import Matchmaking.Common
import Matchmaking.DB
import Matchmaking.Scraper

fetchGrandmasters :: [Task]
fetchGrandmasters =
    [ FetchGrandmasters NA
    , FetchGrandmasters EU
    ]

parseHtml :: IO ()
parseHtml = do
    args <- getArgs
    case args of
        [s] | ".html" `isSuffixOf` s -> do
            contents <- L.readFile s
            mapM_ print $ extractPlayers contents
            exitSuccess
        _ -> return ()

setUpEkg :: IO ()
setUpEkg = do
    store <- serverMetricStore <$> forkServer "0.0.0.0" 8000
    registerGauge "recent potato matches" (toEnum . fst <$> readIORef mmStats) store
    registerGauge "recent matches" (toEnum . snd <$> readIORef mmStats) store

setUpScrapers :: IO ()
setUpScrapers = do
    warmupTasks <- parseArgs =<< getArgs
    conn <- connectPostgreSQL "user=matchmaking dbname=matchmaking"
    entry <- loadPersist conn
    putStrLn $ "Starting scraping from " ++ show entry
    let tasks = warmupTasks ++ map FetchLastMatch [entry ..] ++ allTasks
        allTasks = map FetchLastMatch (enumToPred entry) ++ tasks
    void $ forkIO $ scraper conn tasks
    where
    parseArgs [na, eu] = do
        playersFromFile NA na
        playersFromFile EU eu
        return []
    parseArgs _ = return fetchGrandmasters

main :: IO ()
main = do
    parseHtml
    setUpEkg
    setUpScrapers
    hFlush stdout
    scottyOpts scottySettings $ do
        get "/" rootApp
        get "/csv" csvApp
    where
    scottySettings = Options 0 warpSettings
    warpSettings = setHost "127.0.0.1" $ setPort 3000 defaultSettings
