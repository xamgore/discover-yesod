{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.DiscoverApi where

import Import
import Database.Persist.Sqlite
import GHC.Generics ()
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Aeson 
import Data.Aeson.Types
import Data.Text.Read
import Text.Read
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.), select, from, in_, where_, valList)
import           Web.Cookie
import           Network.HTTP.Simple




getEntity entityId = do
    entity <- runDB $ get404 entityId
    returnJson entity

textToSqlKey t = case decimal t of
                        Right (key, _) -> Just $ toSqlKey $ fromInteger $ key
                        otherwise -> Nothing

pageOptions :: Maybe Text -> Maybe Text -> [SelectOpt val]
pageOptions pageSizeMaybe pageNumberMaybe = 
    case (map decimal pageSizeMaybe, map decimal pageNumberMaybe) of
        (Just (Right (size, _)), Just (Right (number, _))) -> [OffsetBy $ size * number, 
                                                               LimitTo size]
        otherwise -> []

getUsersR :: Handler Value
getUsersR = do
    pageSizeMaybe   <- lookupGetParam "page[size]"
    pageNumberMaybe <- lookupGetParam "page[number]"
    userMaybe <- lookupGetParam "friends_of"
    users <- runDB $ let filters = []
                         options :: [SelectOpt User]
                         options = pageOptions pageSizeMaybe pageNumberMaybe
                      in selectList filters options
    returnJson users

getUserR :: UserId -> Handler Value
getUserR = getEntity

getPlacesR :: Handler Value
getPlacesR = do
    pageSizeMaybe   <- lookupGetParam "page[size]"
    pageNumberMaybe <- lookupGetParam "page[number]"
    positionXMaybe  <- lookupGetParam "coords[x]"
    positionYMaybe  <- lookupGetParam "coords[y]"
    userMaybe       <- lookupGetParam "visited_by"
    places <- runDB $ let filters = case userMaybe of
                            Just user -> []
                            otherwise -> []
                          options :: [SelectOpt Place]
                          options = pageOptions pageSizeMaybe pageNumberMaybe
                       in selectList [] options
    -- returnJson places

    returnJson [
        (Place "Мехмат" 47.216674 39.628794 "Институт математики, механики и компьютерных наук им. Воровича" 
               "https://upload.wikimedia.org/wikipedia/commons/9/9a/%D0%A4%D0%B0%D0%BA%D1%83%D0%BB%D1%8C%D1%82%D0%B5%D1%82_%D0%BC%D0%B0%D1%82%D0%B5%D0%BC%D0%B0%D1%82%D0%B8%D0%BA%D0%B8,_%D0%BC%D0%B5%D1%85%D0%B0%D0%BD%D0%B8%D0%BA%D0%B8_%D0%B8_%D0%BA%D0%BE%D0%BC%D0%BF%D1%8C%D1%8E%D1%82%D0%B5%D1%80%D0%BD%D1%8B%D1%85_%D0%BD%D0%B0%D1%83%D0%BA_%D0%AE%D0%A4%D0%A3.jpg")
        ]

data PlaceMetadata =
     PlaceMetadata { name :: !Text
                   , desc :: !Text
                   , x    :: Double
                   , y    :: Double
              } deriving (Show, Generic)

instance FromJSON PlaceMetadata 
instance ToJSON PlaceMetadata 


postPlacesR :: Handler Value
postPlacesR = do
    contentsMaybe <- lookupFile "contents"
    fileMaybe <- lookupPostParam "name"
    descMaybe <- lookupPostParam "desc"
    xMaybe <- lookupPostParam "x"
    yMaybe <- lookupPostParam "y"
    case (contentsMaybe, fileMaybe, descMaybe, xMaybe, yMaybe) of
        (Just contents, Just file, Just desc, Just x, Just y) -> do
            path <- writeToServer contents
            let place = (Place "a" --()
                               2.3 --()
                               3.3 --()
                               "b" --()
                               (pack path))
            insertedPlace <- runDB $ insertEntity place
            returnJson insertedPlace
        otherwise -> do
            sendResponseStatus status400 emptyObject

imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "static/shared"

writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
    let filename = unpack $ fileName file
        path = imageFilePath filename
    liftIO $ fileMove file path
    return filename

getPlaceR :: PlaceId -> Handler Value
getPlaceR = getEntity

postDiscoveriesR :: Handler Value
postDiscoveriesR = do
    discovery <- (requireJsonBody :: Handler Discovery)
    insertedDiscovery <- runDB $ insertEntity discovery
    returnJson insertedDiscovery

getDiscoveriesR :: Handler Value
getDiscoveriesR = do
    pageSizeMaybe   <- lookupGetParam "page[size]"
    pageNumberMaybe <- lookupGetParam "page[number]"
    userMaybe <- lookupGetParam "user"
    places <- runDB $ let filters = case userMaybe >>= textToSqlKey of
                            Just user -> [DiscoveryUser ==. user]
                            otherwise -> []
                          options :: [SelectOpt Discovery]
                          options = pageOptions pageSizeMaybe pageNumberMaybe
                       in selectList filters options
    returnJson places

getDiscoveryR :: DiscoveryId -> Handler Value
getDiscoveryR = getEntity

getPlacesPhotosR :: PlaceId -> Handler Value
getPlacesPhotosR placeId = do
    photos <- runDB $ selectList [PhotoPlace ==. placeId] []
    returnJson photos

postPlacesPhotosR :: PlaceId -> Handler Value
postPlacesPhotosR placeId = do
    photo <- (requireJsonBody :: Handler Photo)
    insertedPhoto <- runDB $ insertEntity photo
    returnJson insertedPhoto


data VkUser =
  VkUser { online       :: Int
         , first_name   :: !Text
         , last_name    :: !Text
         , photo_medium :: !Text
         , user_id :: Int
           } deriving (Show, Generic)

instance FromJSON VkUser 
instance ToJSON VkUser 


data VkResponse =
  VkResponse { response :: [VkUser] } deriving (Show, Generic)

instance FromJSON VkResponse 
instance ToJSON VkResponse 


getVkFriends :: Value -> IO (Result VkResponse)
getVkFriends = return . fromJSON


-- data UserLeaderboard =
--   UserLeaderboard { first_name :: !Text
--                   , last_name :: !Text
--                   , photo_medium :: !Text
--                   , user_id :: Int
--                   , discoveries :: Int 
--                   } deriving (Show, Generic)

data UserLeaderboardDbData =
  UserLeaderboardDbData { access_token_ :: !Text } deriving (Show, Generic)

instance FromJSON UserLeaderboardDbData
instance ToJSON UserLeaderboardDbData 

-- users :: UserId -> Handler [Entity UserLeaderboardDbData]
-- users uid =
--   runDB $ rawSql
--     "SELECT  " ++
--     "FROM  user " ++
--     "WHERE id = 12605637"
--     []


getLeaderboardR :: Handler Value
getLeaderboardR = do
    maybeUserId <- lookupCookie "user_id"
    case (maybeUserId >>= textToSqlKey) :: Maybe UserId of 
        Just id -> do
            user <- runDB $ get404 id
            let UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = uid}} = id
            req  <- parseRequest $ 
                            "GET https://api.vk.com/method/friends.get?user_id=" ++ show uid ++
                            "&fields=first_name,last_name,photo_medium&access_token=" ++ userToken user
            res <- httpJSON req

            let body = (getResponseBody res :: Value)
    
            
            vkRes <- liftIO $ do
                getVkFriends body

            case vkRes of
                Success vkResponse -> do 
                    let ids  = map user_id (response vkResponse)
                    -- runDB $ E.select $ 
                    --                from $ \user -> do
                    --                     -- where_ ((user ^. UserId) >. 0) --in_ valList ids)
                    --                     return (user :: User)
                    -- returnJson ret
                    -- liftIO $ print ret
                    sendResponseStatus status200 (TypedContent "text/html" "200")
                _ -> sendResponseStatus status500 (TypedContent "text/html" "500 Internal Server Error")
            -- returnJson body

            -- returnJson (results body)
        Nothing -> sendResponseStatus status401 (TypedContent "text/html" "401 Unauthorized")
