module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (size, src)
import Url exposing (Url)


type alias Model =
    { key : Nav.Key
    , url : Url
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Sample - GitHub stars"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div []
        [ h1 [] [ text "Explore GitHub users and Repos" ]
        , div []
            [ p [] [ text "Type username or repo full name and hit 'Go':" ]
            , input [ size 45 ] []
            , button [] [ text "Go!" ]
            ]
        ]
