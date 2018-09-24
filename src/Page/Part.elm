module Page.Part exposing (viewLoadMore)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msg exposing (Msg)
import Paginations as Pgs exposing (Pgs)


viewLoadMore : (String -> Msg) -> String -> Pgs v -> Html Msg
viewLoadMore msg key pgs =
    if Pgs.isFetching key pgs then
        div [ class "load-more-loading" ] [ text "Loading..." ]

    else
        case Pgs.nextPageUrl key pgs of
            Just url ->
                button [ type_ "button", class "load-more-btn", onClick (msg url) ]
                    [ text "Load more" ]

            Nothing ->
                span [] []
