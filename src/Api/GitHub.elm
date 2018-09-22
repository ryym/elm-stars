module Api.GitHub exposing (StarredList, nextPageUrl, repository, starred, starredMore, user)

import Api.Http exposing (Error(..), JsonResult, Response, getJson)
import Debug
import Dict
import Http exposing (Request)
import Json.Decode as J exposing (Decoder, field)
import Repo exposing (Repo)
import Task
import Url
import Url.Builder exposing (QueryParameter)
import User exposing (User)


type alias ResponseResult a =
    Result Error (Response a)


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


{-| Header format:
Link: <url>; rel="next", <url>; rel="last"
-}
nextPageUrl : Http.Response a -> Maybe String
nextPageUrl res =
    Dict.get "link" res.headers
        |> Maybe.map (String.split ",")
        |> Maybe.andThen (List.filter (String.contains "rel=\"next\"") >> List.head)
        |> Maybe.andThen (String.split ";" >> List.head)
        |> Maybe.map (String.trim >> String.slice 1 -1)


send : (Result Error (Response a) -> msg) -> Request (JsonResult a) -> Cmd msg
send msg =
    Http.send (flatResult >> msg)


user : (Result Error (Response User) -> msg) -> String -> Cmd msg
user msg name =
    getJson (url [ "users", name ] []) User.decoder |> send msg


starred : (Result Error (Response StarredList) -> msg) -> String -> Cmd msg
starred msg name =
    getJson (url [ "users", name, "starred" ] []) repoListDecoder |> send msg


starredMore : (Result Error (Response StarredList) -> msg) -> String -> Cmd msg
starredMore msg nextUrl =
    getJson nextUrl repoListDecoder |> send msg


repository : (Result Error (Response ( Repo, User )) -> msg) -> String -> Cmd msg
repository msg name =
    getJson (url [ "repos", name ] []) repoDecoder |> send msg
