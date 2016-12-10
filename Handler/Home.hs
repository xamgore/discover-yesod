module Handler.Home where

import Import


getHomeR :: Handler ()
getHomeR = sendFile "text/html" "static/index.html"
