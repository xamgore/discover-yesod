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
module Handler.DiscoverApi where

import Import
import Database.Persist.Sqlite
import GHC.Generics ()
import Data.Aeson ()
import Data.Aeson.Types ()
import Data.Text.Read
import Text.Read
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
    returnJson places

postPlacesR :: Handler Value
postPlacesR = do
    place <- (requireJsonBody :: Handler Place)
    insertedPlace <- runDB $ insertEntity place
    returnJson insertedPlace

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

data Person =
  Person { firstName  :: !Text
         , lastName   :: !Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving Show

getLeaderboardR :: Handler Value
getLeaderboardR = do
    maybeUserId <- lookupCookie "user_id"
    case (maybeUserId >>= textToSqlKey) :: Maybe UserId of 
        Just id -> do
            user <- runDB $ get404 id
            let UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = uid}} = id
                in do
                    request  <- parseRequest $ 
                                    "GET https://api.vk.com/method/friends.get?user_id=" ++ show uid ++
                                    "&fields=uid,first_name,last_name,photo_medium&access_token=" ++ userToken user
                    response <- httpJSON request

                    let body  = getResponseBody response :: Value
                    returnJson body
            -- let body  = getResponseBody response :: Value
            -- select $ from $ \user -> do
            --         where_ $ user ^. UserId in_ valList body
            --         return user
        Nothing -> sendResponseStatus status401 (TypedContent "text/html" "401 Unauthorized")
