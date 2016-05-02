module Matchmaking.Scraper (scraper) where

import Database.PostgreSQL.Simple

import Matchmaking.Common

scraper :: Connection -> [Task] -> IO ()
scraper _conn _tasks = return ()
