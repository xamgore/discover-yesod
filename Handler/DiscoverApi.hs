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

getEntity entityId = do
    entity <- runDB $ get404 entityId
    returnJson entity

getUsersR :: Handler Value
getUsersR = do
    pageMaybe <- lookupGetParam "page"
    userMaybe <- lookupGetParam "friends_of"
    users <- runDB $ selectList [] [Asc UserId]
    returnJson users

getUserR :: UserId -> Handler Value
getUserR = getEntity

getPlacesR :: Handler Value
getPlacesR = do
    pageMaybe <- lookupGetParam "page"
    positionMaybe <- lookupGetParam "nearby_xy"
    userMaybe <- lookupGetParam "visited_by"
    places <- runDB $ selectList [] [Asc PlaceId]
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
    pageMaybe <- lookupGetParam "page"
    userMaybe <- lookupGetParam "user"
    places <- case userMaybe of
        Just user -> runDB $ selectList [DiscoveryUser ==. toSqlKey 1] [Asc DiscoveryId]
        Nothing   -> runDB $ selectList [] [Asc DiscoveryId]
    returnJson places

getDiscoveryR :: DiscoveryId -> Handler Value
getDiscoveryR = getEntity

getPlacesPhotosR :: PlaceId -> Handler Value
getPlacesPhotosR placeId = undefined

postPlacesPhotosR :: PlaceId -> Handler Value
postPlacesPhotosR placeId = undefined

