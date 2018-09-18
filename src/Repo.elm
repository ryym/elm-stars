module Repo exposing (Repo, decoder)

import Json.Decode as J exposing (Decoder, field)


type alias Repo =
    { name : String
    , fullName : String
    , description : Maybe String
    , ownerName : String
    }


decoder =
    J.map4 Repo
        (field "name" J.string)
        (field "full_name" J.string)
        (field "description" (J.nullable J.string))
        (J.at [ "owner", "login" ] J.string)
