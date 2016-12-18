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
import Text.Read

getEntity entityId = do
    entity <- runDB $ get404 entityId
    returnJson entity


-- todo: safe parsing
pageOptions :: Maybe Text -> Maybe Text -> [SelectOpt val]
pageOptions pageSizeMaybe pageNumberMaybe = 
    case (pageSizeMaybe, pageNumberMaybe) of
        (Just size, Just number) -> let sizeNum   = read $ unpack size 
                                        numberNum = read $ unpack number
                                      in [OffsetBy (sizeNum * numberNum), 
                                          LimitTo sizeNum]
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
    places <- runDB $ let filters = case userMaybe of
                            Just user -> [DiscoveryUser ==. toSqlKey (read $ unpack user)]
                            otherwise -> []
                          options :: [SelectOpt Discovery]
                          options = pageOptions pageSizeMaybe pageNumberMaybe
                       in selectList filters options
    returnJson places

getDiscoveryR :: DiscoveryId -> Handler Value
getDiscoveryR = getEntity

getPlacesPhotosR :: PlaceId -> Handler Value
getPlacesPhotosR placeId = undefined

postPlacesPhotosR :: PlaceId -> Handler Value
postPlacesPhotosR placeId = undefined

