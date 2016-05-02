module Matchmaking.App (rootApp) where

import Control.Monad.IO.Class
import Data.IORef
import Data.String
import Text.Blaze.Html5 ((!), Html)
import Text.Blaze.Html.Renderer.Text
import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Matchmaking.Common

threshold :: Double
threshold = 0.1

answer :: Int -> Int -> Html
answer _ 0 = H.p "DUNNO... the website is broken"
answer np na
    | ratio > threshold = broken
    | otherwise = fixed
    where
    ratio = toEnum np / toEnum na
    percentage :: Int
    percentage = round $ 100 * ratio
    detail = H.p (fromString $ "In " ++ show percentage ++ "% of high level matches played during prime time the difference between best and worst player exceeds 1000 hotdogs.")
    broken = H.p "NO, matchmaking is still broken" >> detail
    fixed = H.p "YES, matchmaking is working well" >> detail

rootApp :: ActionM ()
rootApp = do
    (nPotato, nAll) <- liftIO $ readIORef mmStats
    html $ renderHtml $ H.docTypeHtml $ do
        H.head $ H.title "Is Matchmaking Fixed Yet?"
        H.body $ do
            answer nPotato nAll
            H.p $ H.a "Created by Bartek Gąsiorzewski" ! A.href "https://github.com/HotChick91/matchmaking"
