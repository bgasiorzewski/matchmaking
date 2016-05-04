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
threshold = 0.3

answer :: Int -> Int -> Html
answer _ 0 = H.p "DUNNO... the website is broken" ! A.id "answer"
answer np na
    | ratio > threshold = broken
    | otherwise = fixed
    where
    ratio = toEnum np / toEnum na
    percentage :: Int
    percentage = round $ 100 * ratio
    pr cl = H.span (fromString $ show percentage ++ "%") ! A.class_ cl
    detail cl = do
        H.p $ "In " >> pr cl >> " of high level matches, the difference between the best and worst player exceeds 1000 hotdogs."
        H.p $ "It was 22% before Tracer's release."
    broken = do
        H.p (H.span "NO" ! A.class_ "no" >> ", matchmaking is still broken") ! A.id "answer"
        detail "number no"
        footer "no"
    fixed = do
        H.p (H.span "YES" ! A.class_ "yes" >> ", matchmaking is working well") ! A.id "answer"
        detail "number yes"
        footer "yes"
    footer cl = H.footer $ do
        _ <- "The \""
        H.span (fromString $ show percentage ++ "%") ! A.class_ cl
        _ <- "\" number is updated in real-time. "
        H.a "Find out how."
            ! A.href "https://github.com/HotChick91/matchmaking#how-does-it-work"

rootApp :: ActionM ()
rootApp = do
    (nPotato, nAll) <- liftIO $ readIORef mmStats
    setHeader "Refresh" "300"
    html $ renderHtml $ H.docTypeHtml $ do
        H.head $ do
            H.title "Is Matchmaking Fixed Yet?"
            H.link
                ! A.href "http://www.ismatchmakingfixedyet.com:8080/matchmaking.css"
                ! A.rel "stylesheet"
            H.link
                ! A.href "http://www.ismatchmakingfixedyet.com:8080/favicon.ico"
                ! A.rel "shortcut icon"
                ! A.type_ "image/x-icon"
            H.meta
                ! H.customAttribute "property" "og:title"
                ! A.content "Is Matchmaking Fixed Yet?"
            H.meta
                ! H.customAttribute "property" "og:type"
                ! A.content "website"
            H.meta
                ! H.customAttribute "property" "og:image"
                ! A.content "http://www.ismatchmakingfixedyet.com:8080/potatoes.png"
            H.meta
                ! H.customAttribute "property" "og:url"
                ! A.content "http://www.ismatchmakingfixedyet.com/"
        H.body $ do
            H.h1 "Is Hero League matchmaking for Heroes of the Storm fixed yet?"
            answer nPotato nAll
            H.script $ mconcat
                [ "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
                , "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
                , "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
                , "})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');"
                , "ga('create', 'UA-77246746-1', 'auto');"
                , "ga('send', 'pageview');"
                ]
