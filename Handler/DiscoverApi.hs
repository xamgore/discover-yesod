module Handler.DiscoverApi where

import Import

getUsersR :: Handler Html
getUsersR = do
    pageMaybe <- lookupGetParam "page"
    userMaybe <- lookupGetParam "friends_of"
    case userMaybe of
        Just user -> sendFile "text/html" ("static/index.html" ++ unpack user)
        Nothing   -> sendFile "text/html" "static/index.html"

getUserR :: Text -> Handler Html
getUserR userId = sendFile "text/html" "static/index.html"

getPlacesR :: Handler Html
getPlacesR = do
    pageMaybe <- lookupGetParam "page"
    positionMaybe <- lookupGetParam "nearby_xy"
    userMaybe <- lookupGetParam "visited_by"
    sendFile "text/html" "static/index.html"

postPlacesR :: Handler Html
postPlacesR = sendFile "text/html" "static/index.html"

getPlaceR :: Text -> Handler Html
getPlaceR placeId = sendFile "text/html" "static/index.html"

postDiscoverPlaceR :: Text -> Handler Html
postDiscoverPlaceR placeId = sendFile "text/html" "static/index.html"

getPlacesPhotosR :: Text -> Handler Html
getPlacesPhotosR placeId = sendFile "text/html" "static/index.html"

postPlacesPhotosR :: Text -> Handler Html
postPlacesPhotosR placeId = sendFile "text/html" "static/index.html"

