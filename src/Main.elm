module Main exposing (main)

import Api.GitHub exposing (StarredList)
import Api.Http exposing (Error, Response(..))
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (alt, class, href, size, src)
import Http
import Repo exposing (Repo)
import Route exposing (Route, toRoute)
import Url exposing (Url)
import User exposing (User)


type alias Model =
    { key : Nav.Key
    , route : Route
    , users : Dict String User
    , repos : Dict String Repo
    , starred : Dict String (List String)
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | UserFetched (Result Error (Response User))
    | StarredListFetched String (Result Error (Response StarredList))


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
        , repos = Dict.empty
        , starred = Dict.empty
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
                        Cmd.batch
                            [ Api.GitHub.user UserFetched name
                            , Api.GitHub.starred (StarredListFetched name) name
                            ]
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
                Ok (Response _ user) ->
                    ( { model | users = Dict.insert user.login user model.users }, Cmd.none )

                Err err ->
                    -- TODO: Handle error.
                    ( model, Cmd.none )

        StarredListFetched userName result ->
            case result of
                Ok (Response res starredList) ->
                    let
                        appendStarred repo starred =
                            case Dict.get userName starred of
                                Just names ->
                                    Dict.insert userName (repo.fullName :: names) starred

                                Nothing ->
                                    Dict.insert userName [ repo.fullName ] starred

                        reduce ( repo, user ) m =
                            { m
                                | users = Dict.insert user.login user m.users
                                , repos = Dict.insert repo.fullName repo m.repos
                                , starred = appendStarred repo m.starred
                            }
                    in
                    ( List.foldl reduce model starredList, Cmd.none )

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
                        [ h2 [] [ text <| user.login ]
                        , img [ src user.avatar.url, alt "", class "user-avatar" ] []
                        , h3 [] [ text "starred repositories" ]
                        , ul [] <| viewStarred name model
                        ]

                Nothing ->
                    div []
                        [ p [] [ text "Loading..." ] ]

        Route.NotFound ->
            div []
                [ p [] [ text "Not Found" ] ]


viewStarred : String -> Model -> List (Html Msg)
viewStarred userName model =
    let
        toRepoList repos repoName acc =
            case Dict.get repoName repos of
                Just repo ->
                    repo :: acc

                Nothing ->
                    acc
    in
    Dict.get userName model.starred
        |> Maybe.withDefault []
        |> List.foldl (toRepoList model.repos) []
        |> List.map (\repo -> li [] [ text repo.fullName ])
