module Paginations exposing (Pgs, empty, finishFetch, getIds, isFetching, startFetch)

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


getIds : String -> Pgs v -> List v
getIds key pgs =
    case Dict.get key pgs of
        Just pg ->
            pg.ids

        Nothing ->
            []


isFetching : String -> Pgs v -> Bool
isFetching key pgs =
    case Dict.get key pgs of
        Just pg ->
            pg.isFetching

        Nothing ->
            True


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
