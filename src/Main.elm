module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, href, size, src)
import Http
import Route exposing (Route, toRoute)
import Url exposing (Url)
import User exposing (User)



-- import  Http


type alias Model =
    { key : Nav.Key
    , route : Route
    , users : Dict String User
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | UserFetched (Result Http.Error User)


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
    initPage
        { key = key
        , route = toRoute url
        , users = Dict.empty
        }


initPage : Model -> ( Model, Cmd Msg )
initPage model =
    case model.route of
        Route.User name ->
            let
                cmd =
                    if Dict.member name model.users then
                        Cmd.none

                    else
                        Http.get ("https://api.github.com/users/" ++ name) User.decoder
                            |> Http.send UserFetched
            in
            ( model, cmd )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            initPage { model | route = toRoute url }

        UserFetched result ->
            case result of
                Ok user ->
                    ( { model | users = Dict.insert user.login user model.users }, Cmd.none )

                Err err ->
                    -- TODO: Handle error.
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
            case Dict.get name model.users of
                Just user ->
                    div []
                        [ h2 [] [ text <| "User : " ++ user.login ]
                        , img [ src user.avatar.url ] []
                        ]

                Nothing ->
                    div []
                        [ p [] [ text "Loading..." ] ]

        Route.NotFound ->
            div []
                [ p [] [ text "Not Found" ] ]
