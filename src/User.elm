module User exposing (User, decoder)

import Json.Decode as J exposing (Decoder, field)


type alias Avatar =
    { url : String }


type alias User =
    { login : String
    , avatar : Avatar
    }


decoder : Decoder User
decoder =
    J.map2 User
        (field "login" J.string)
        (field "avatar_url" <| J.map Avatar J.string)
