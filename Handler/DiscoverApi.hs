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


getUsersR :: Handler Html
getUsersR = do
    pageMaybe <- lookupGetParam "page"
    userMaybe <- lookupGetParam "friends_of"
    case userMaybe of
        Just user -> sendFile "text/html" ("static/index.html" ++ unpack user)
        Nothing   -> sendFile "text/html" "static/index.html"

getUserR :: UserId -> Handler Html
getUserR userId = undefined

getPlacesR :: Handler Value
getPlacesR = do
    pageMaybe <- lookupGetParam "page"
    positionMaybe <- lookupGetParam "nearby_xy"
    userMaybe <- lookupGetParam "visited_by"
    places <- runDB $ selectList [] [Asc PlaceName]
    returnJson places

postPlacesR :: Handler Value
postPlacesR = do
    place <- (requireJsonBody :: Handler Place)
    insertedPlace <- runDB $ insertEntity place
    returnJson insertedPlace

getPlaceR :: PlaceId -> Handler Value
getPlaceR placeId = do
    place <- runDB $ get404 placeId
    returnJson place

postDiscoverPlaceR :: PlaceId -> Handler Html
postDiscoverPlaceR placeId = undefined

getPlacesPhotosR :: PlaceId -> Handler Html
getPlacesPhotosR placeId = undefined

postPlacesPhotosR :: PlaceId -> Handler Html
postPlacesPhotosR placeId = undefined

