module Api.GitHub exposing (StarredList, starred, user)

import Api.Http exposing (Error(..), JsonResult, Response, getJson)
import Http exposing (Request)
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


flatResult : Result Http.Error (JsonResult a) -> Result Error (Response a)
flatResult ret =
    case ret of
        Ok jret ->
            Result.mapError (\e -> JsonError e) jret

        Err err ->
            Err (HttpError err)


send : (Result Error (Response a) -> msg) -> Request (JsonResult a) -> Cmd msg
send msg =
    Http.send (flatResult >> msg)


user : (Result Error (Response User) -> msg) -> String -> Cmd msg
user msg name =
    getJson (url [ "users", name ] []) User.decoder |> send msg


starred : (Result Error (Response StarredList) -> msg) -> String -> Cmd msg
starred msg name =
    getJson (url [ "users", name, "starred" ] []) repoListDecoder |> send msg
