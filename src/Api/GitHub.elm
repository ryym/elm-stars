module Api.GitHub exposing (user)

import Http
import Url.Builder exposing (QueryParameter)
import User exposing (User)


url : List String -> List QueryParameter -> String
url paths query =
    Url.Builder.crossOrigin "https://api.github.com" paths query


user : (Result Http.Error User -> msg) -> String -> Cmd msg
user msg name =
    Http.send msg <| Http.get (url [ "users", name ] []) User.decoder
