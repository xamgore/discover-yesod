User json
    ident Text
    password Text Maybe
    UniqueUser ident
    deriving Typeable
    deriving Show

Email
    email Text
    userId UserId Maybe
    verkey Text Maybe
    UniqueEmail email
    
Comment json -- Adding "json" causes ToJSON and FromJSON instances to be derived.
    message Text
    userId UserId Maybe
    deriving Eq
    deriving Show

Place json
    name Text
    x Double
    y Double
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

UserAuthData json
    user UserId
    token Text
    deriving Show

 -- By default this file is used in Model.hs (which is imported by Foundation.hs)

