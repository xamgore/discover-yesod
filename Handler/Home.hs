{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Home where

import Import
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL (toStrict)
import           Data.Text.Lazy.Builder      (toLazyText)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Data.Aeson
import           Data.Aeson.Encode (encodeToTextBuilder)
import           Network.HTTP.Simple
-- https://haskell-lang.org/library/http-client
-- https://vk.com/dev/authcode_flow_user


data VkAuth = VkAuth { access_token :: String, expires_in :: Int, user_id :: Int } deriving (Generic, Show)
instance FromJSON VkAuth


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

            (Success vk) <- liftIO $ makeRequest url
            friendsList  <- liftIO $ getFriends vk
            boardTempl   <- liftIO $ readFile "static/board.html"

            addHeader "Content-Type" "text/html"
            sendResponse $ boardTempl ++ "<script>init(" ++ (T.unpack friendsList) ++ ");</script>"


makeRequest :: String -> IO (Result VkAuth)
makeRequest url = do
    request  <- parseRequest $ "GET " ++ url
    response <- httpJSON request

    let body = getResponseBody response :: Value
    S8.putStrLn $ Yaml.encode body
    return $ fromJSON body


getFriends :: VkAuth -> IO Text
getFriends vkAuth = do
    request  <- parseRequest $ "GET https://api.vk.com/method/friends.get?user_id=" ++ (show $ user_id vkAuth) ++
                "&order=hints&count=10&offset=0&fields=uid,first_name,last_name,photo_medium&access_token=" ++
                (access_token vkAuth)
    response <- httpJSON request
    let body  = getResponseBody response :: Value

    return (valToText body)


vkapi :: Int -> String -> String -> String -> String
vkapi client secret back code =
    "https://oauth.vk.com/access_token?client_id=" ++ show client ++
    "&client_secret=" ++ secret ++ "&redirect_uri=" ++ back ++ "&code=" ++ code

valToText :: Value -> Text
valToText = TL.toStrict . toLazyText . encodeToTextBuilder
