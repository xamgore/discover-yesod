{-# LANGUAGE OverloadedStrings #-}

module Handler.Home where

import Import
import qualified Data.Text             as T
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
-- https://haskell-lang.org/library/http-client


getHomeR :: Handler ()
getHomeR = do
    codeMaybe <- lookupGetParam "code"
    print codeMaybe

    case (T.unpack <$> codeMaybe) of
        Nothing   -> sendFile "text/html" "static/index.html"
        Just code -> do
            let url = vkapi 5781698 "plMjp5oUptPh4QKxAfkz" "http://127.0.0.1:3000" code
            print url
            putStrLn ""

            res <- liftIO $ makeRequest url
            sendResponse res


makeRequest :: String -> IO Value
makeRequest url = do
    request  <- parseRequest $ "GET " ++ url
    response <- httpJSON request

    -- putStrLn $ "The status code was: " ++
    --            (T.pack $ show (getResponseStatusCode response))
    -- print $ getResponseHeader "Content-Type" response

    let body = getResponseBody response :: Value
    S8.putStrLn $ Yaml.encode body
    return body


vkapi :: Int -> String -> String -> String -> String
vkapi client secret back code =
    "https://oauth.vk.com/access_token?client_id=" ++ show client ++
    "&client_secret=" ++ secret ++ "&redirect_uri=" ++ back ++ "&code=" ++ code
