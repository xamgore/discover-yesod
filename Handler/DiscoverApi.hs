module Handler.DiscoverApi where

import Import

getUsersR :: Handler ()
getUsersR = sendFile "text/html" "static/index.html"

getUserR :: Text -> Handler ()
getUserR userId = sendFile "text/html" "static/index.html"

getPlacesR :: Handler ()
getPlacesR = sendFile "text/html" "static/index.html"

postPlacesR :: Handler ()
postPlacesR = sendFile "text/html" "static/index.html"

getPlaceR :: Text -> Handler ()
getPlaceR placeId = sendFile "text/html" "static/index.html"

postDiscoverPlaceR :: Text -> Handler ()
postDiscoverPlaceR placeId = sendFile "text/html" "static/index.html"

getPlacesPhotosR :: Text -> Handler ()
getPlacesPhotosR placeId = sendFile "text/html" "static/index.html"

postPlacesPhotosR :: Text -> Handler ()
postPlacesPhotosR placeId = sendFile "text/html" "static/index.html"

