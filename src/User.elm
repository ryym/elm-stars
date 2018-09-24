module User exposing (User, UserName, decoder)

import Json.Decode as J exposing (Decoder, field)


type alias UserName =
    String


type alias Avatar =
    { url : String }


type alias User =
    { login : UserName
    , avatar : Avatar
    }


decoder : Decoder User
decoder =
    J.map2 User
        (field "login" J.string)
        (field "avatar_url" <| J.map Avatar J.string)
