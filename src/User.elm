module User exposing (User, decoder)

import Json.Decode as J exposing (Decoder, field)


type alias Avatar =
    { url : String }


type alias User =
    { login : String
    , name : String
    , avatar : Avatar
    }


decoder : Decoder User
decoder =
    J.map3 User
        (field "login" J.string)
        (field "name" J.string)
        (field "avatar_url" <| J.map Avatar J.string)
