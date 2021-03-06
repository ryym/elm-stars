module Api.Http exposing (Error(..), JsonResult, Response(..), getJson, showError)

import Http exposing (Error(..), Expect, Request, expectStringResponse, header)
import Json.Decode as J exposing (Decoder)


type Error
    = HttpError Http.Error
    | JsonError J.Error


showError : Error -> String
showError err =
    case err of
        HttpError e ->
            "Http Error: " ++ showHttpError e

        JsonError e ->
            "Json Error: " ++ J.errorToString e


showHttpError : Http.Error -> String
showHttpError err =
    case err of
        BadUrl url ->
            "Bad URL " ++ url

        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        BadStatus res ->
            "BadStatus " ++ res.status.message

        BadPayload msg _ ->
            "BadPayload " ++ msg


type Response a
    = Response (Http.Response String) a


type alias JsonResult a =
    Result J.Error (Response a)


getJson : String -> Decoder a -> Request (JsonResult a)
getJson url decoder =
    Http.request
        { method = "GET"
        , headers = [ header "Content-Type" "application/json" ]
        , url = url
        , body = Http.emptyBody
        , timeout = Nothing
        , withCredentials = False
        , expect = expectResponse decoder
        }


expectResponse : Decoder a -> Expect (JsonResult a)
expectResponse decoder =
    let
        decodeBody res =
            J.decodeString decoder res.body
                |> Result.map (Response res)
                |> Ok
    in
    expectStringResponse decodeBody
