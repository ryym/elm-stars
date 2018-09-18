module Api.GitHub exposing (StarredList, starred, user)

import Api.Http
import Http
import Json.Decode as J exposing (Decoder, field)
import Repo exposing (Repo)
import Task
import Url.Builder exposing (QueryParameter)
import User exposing (User)


type alias StarredList =
    List ( Repo, User )


repoDecoder : Decoder ( Repo, User )
repoDecoder =
    J.map2 (\repo owner -> ( repo, owner ))
        Repo.decoder
        (field "owner" User.decoder)


repoListDecoder : Decoder StarredList
repoListDecoder =
    J.list repoDecoder


url : List String -> List QueryParameter -> String
url paths query =
    Url.Builder.crossOrigin "https://api.github.com" paths query


user : (Result Http.Error User -> msg) -> String -> Cmd msg
user msg name =
    Http.send msg <| Http.get (url [ "users", name ] []) User.decoder


starred : (Result Http.Error StarredList -> msg) -> String -> Cmd msg
starred msg name =
    Http.send msg <| Http.get (url [ "users", name, "starred" ] []) repoListDecoder
