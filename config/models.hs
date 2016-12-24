User json
    token String
    deriving Show

Place json
    name Text
    x Double
    y Double
    desc Text
    image Text
    deriving Show

Photo json
    place PlaceId
    url Text
    deriving Show

Discovery json
    user UserId
    place PlaceId
    time Text
    deriving Show

 -- By default this file is used in Model.hs (which is imported by Foundation.hs)

