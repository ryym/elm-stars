module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, href, size, src)
import Route exposing (Route, toRoute)
import Url exposing (Url)


type alias Model =
    { key : Nav.Key
    , route : Route
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
    ( Model key (toRoute url), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = toRoute url }, Cmd.none )


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
            , nav [ class "header-nav" ]
                [ a [ href "/" ] [ text "top" ]
                , a [ href "/ryym" ] [ text "ryym" ]
                , a [ href "/this/is/invalid/path" ] [ text "404" ]
                ]
            , hr [] []
            , viewPage model
            ]
        ]


viewPage : Model -> Html Msg
viewPage model =
    case model.route of
        Route.Home ->
            div []
                [ p [] [ text "This is Home" ] ]

        Route.User name ->
            div []
                [ h2 [] [ text <| "User : " ++ name ] ]

        Route.NotFound ->
            div []
                [ p [] [ text "Not Found" ] ]
