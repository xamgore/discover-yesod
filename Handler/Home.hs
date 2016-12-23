{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Home where

import Import
import           Web.Cookie
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL (toStrict)
import           Data.Text.Lazy.Builder      (toLazyText)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Data.Aeson
import           Data.Aeson.Encode (encodeToTextBuilder)
import           Network.HTTP.Simple
import Database.Persist.Sqlite

-- https://haskell-lang.org/library/http-client
-- https://vk.com/dev/authcode_flow_user
data VkAuth = VkAuth { access_token :: String, expires_in :: Int, user_id :: Int } deriving (Generic, Show)
instance FromJSON VkAuth


vkapi :: Int -> String -> String -> String -> String
vkapi client secret back code =
    "https://oauth.vk.com/access_token?client_id=" ++ show client ++
    "&client_secret=" ++ secret ++ "&redirect_uri=https://" ++ back ++ "&code=" ++ code


getHomeR :: Handler ()
getHomeR = do
    maybeUserId <- lookupCookie "user_id"

    case maybeUserId of
        Just _   -> sendFile "text/html" "static/view.html"
        Nothing  -> authorizeUser

toUserId :: VkAuth -> UserId
toUserId = toSqlKey . fromIntegral . user_id

saveUser :: VkAuth -> Handler ()
saveUser vk = do
    runDB $ deleteWhere [UserId ==. toUserId vk]
    runDB $ insert (UserAuthData (toUserId vk) (access_token vk))
    return ()

authorizeUser :: Handler ()
authorizeUser = do
    maybeCode <- fmap T.unpack <$> lookupGetParam "code"

    case maybeCode of
        Nothing   -> sendFile "text/html" "static/auth.html"
        Just code -> do
            (Just host) <- lookupHeader "host"

            let url = vkapi 5781698 "plMjp5oUptPh4QKxAfkz" (S8.unpack host) code
            vkRes <- liftIO $ getVkAuthToken url

            case vkRes of
                Success vk -> do
                    setCookie $ def { setCookieName = "user_id", setCookieValue = S8.pack $ show $ user_id vk }
                    saveUser vk
                _ ->
                    deleteCookie "user_id" ""

            redirect HomeR




valToText :: Value -> Text
valToText = TL.toStrict . toLazyText . encodeToTextBuilder


getVkAuthToken :: String -> IO (Result VkAuth)
getVkAuthToken url = do
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
