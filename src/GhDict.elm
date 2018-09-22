module GhDict exposing (GhDict, empty, get, insert, member, normalize, toDict, update)

import Dict exposing (Dict)



-- GitHub APIs of users and repositories are case insensitive
-- so we need to be so as well. For example, a user "someUser"
-- can be fetched by "/SOMEUSER", "/someuser", "/SoMeUsEr", etc.


type GhDict v
    = GhDict (Dict String v)


normalize : String -> String
normalize =
    String.toLower


empty : GhDict v
empty =
    GhDict Dict.empty


member : String -> GhDict v -> Bool
member key =
    toDict >> Dict.member (normalize key)


get : String -> GhDict v -> Maybe v
get key =
    toDict >> Dict.get (normalize key)


update : String -> (Maybe v -> Maybe v) -> GhDict v -> GhDict v
update key =
    map << Dict.update (normalize key)


insert : String -> v -> GhDict v -> GhDict v
insert key =
    map << Dict.insert (normalize key)


map : (Dict String a -> Dict String b) -> GhDict a -> GhDict b
map f (GhDict dict) =
    GhDict (f dict)


toDict : GhDict v -> Dict String v
toDict (GhDict dict) =
    dict
