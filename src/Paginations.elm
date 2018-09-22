module Paginations exposing (Pgs, empty, finishFetch, getIds, isFetching, nextPageUrl, startFetch)

import Dict exposing (Dict)
import Pagination as Pg exposing (Pg)


type alias Pgs v =
    Dict String (Pg v)


empty : Pgs v
empty =
    Dict.empty


get : String -> Pgs v -> Pg v
get key =
    Maybe.withDefault Pg.empty << Dict.get key


getValue : (Pg v -> a) -> a -> String -> Pgs v -> a
getValue f default key pgs =
    Dict.get key pgs |> Maybe.map f |> Maybe.withDefault default


getIds : String -> Pgs v -> List v
getIds =
    getValue .ids []


isFetching : String -> Pgs v -> Bool
isFetching =
    getValue .isFetching True


nextPageUrl : String -> Pgs v -> Maybe String
nextPageUrl =
    getValue .nextPageUrl Nothing


startFetch : String -> Pgs v -> Pgs v
startFetch key =
    Dict.update key
        (\pg ->
            Maybe.withDefault Pg.empty pg
                |> Pg.startFetch
                |> Just
        )


finishFetch : String -> ( List v, Maybe String ) -> Pgs v -> Pgs v
finishFetch key ( ids, url ) =
    Dict.update key
        (\pg ->
            Maybe.withDefault Pg.empty pg
                |> Pg.finishFetch ids url
                |> Just
        )
