module Main exposing (main)

import Api.GitHub as GitHub exposing (StarredList)
import Api.Http exposing (Error, Response(..), showError)
import Browser
import Browser.Navigation as Nav
import Debug
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (alt, class, href, size, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Paginations as Pgs exposing (Pgs)
import Repo exposing (Repo)
import Route exposing (Route, toRoute)
import Url exposing (Url)
import User exposing (User)



-- TODO: Define UserName and RepoName types for clarity.


type alias Model =
    { key : Nav.Key
    , route : Route
    , query : String
    , users : Dict String User
    , repos : Dict String Repo
    , starred : Pgs String
    , errMsg : Maybe String
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | InputQuery String
    | Search
    | UserFetched (Result Error (Response User))
    | StarredListFetched String (Result Error (Response StarredList))
    | WantMoreStarred String String


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
        , query = ""
        , users = Dict.empty
        , repos = Dict.empty
        , starred = Pgs.empty
        , errMsg = Nothing
        }


initPage : Model -> ( Model, Cmd Msg )
initPage model =
    case model.route of
        Route.Home ->
            ( { model | query = "" }, Cmd.none )

        Route.User name ->
            let
                fetchUser =
                    if Dict.member name model.users then
                        Cmd.none

                    else
                        GitHub.user UserFetched name

                fetchStarred =
                    if Dict.member name model.starred then
                        Cmd.none

                    else
                        GitHub.starred (StarredListFetched name) name
            in
            ( { model
                | starred = Pgs.startFetch name model.starred
                , query = name
              }
            , Cmd.batch [ fetchUser, fetchStarred ]
            )

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

        InputQuery q ->
            ( { model | query = q }, Cmd.none )

        Search ->
            ( model, Nav.pushUrl model.key ("/" ++ model.query) )

        UserFetched result ->
            case result of
                Ok (Response _ user) ->
                    ( { model | users = Dict.insert user.login user model.users }, Cmd.none )

                Err err ->
                    ( { model | errMsg = Just (showError err) }, Cmd.none )

        StarredListFetched userName result ->
            case result of
                Ok (Response res starredList) ->
                    let
                        reduce m =
                            List.foldl mergeBoth m starredList |> finishFetch

                        mergeBoth ( repo, user ) m =
                            { m
                                | users = Dict.insert user.login user m.users
                                , repos = Dict.insert repo.fullName repo m.repos
                            }

                        finishFetch m =
                            { m | starred = Pgs.finishFetch userName ( repoNames, nextPageUrl ) m.starred }

                        nextPageUrl =
                            GitHub.nextPageUrl res

                        repoNames =
                            List.map (\( repo, _ ) -> repo.fullName) starredList
                    in
                    ( reduce model, Cmd.none )

                Err err ->
                    ( { model | errMsg = Just (showError err) }, Cmd.none )

        WantMoreStarred userName nextUrl ->
            ( { model | starred = Pgs.startFetch userName model.starred }
            , GitHub.starredMore (StarredListFetched userName) nextUrl
            )


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
            , form [ onSubmit Search ]
                [ input [ size 45, onInput InputQuery, value model.query ] []
                , button [ type_ "submit" ] [ text "Go!" ]
                ]
            , nav [ class "header-nav" ]
                [ a [ href "/" ] [ text "top" ]
                , a [ href "/ryym" ] [ text "ryym" ]
                , a [ href "/this/is/invalid/path" ] [ text "404" ]
                ]
            , viewErrMsg model.errMsg
            , hr [] []
            , viewPage model
            ]
        ]


viewErrMsg : Maybe String -> Html Msg
viewErrMsg maybe =
    case maybe of
        Just msg ->
            p [] [ text msg ]

        Nothing ->
            span [] []


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
                        , viewStarred name model
                        ]

                Nothing ->
                    div []
                        [ p [] [ text "Loading..." ] ]

        Route.NotFound ->
            div []
                [ p [] [ text "Not Found" ] ]


viewStarred : String -> Model -> Html Msg
viewStarred userName model =
    div []
        [ ul [] <| viewStarredList userName model
        , viewLoadMore userName model.starred
        ]


viewStarredList : String -> Model -> List (Html Msg)
viewStarredList userName model =
    let
        toRepoList repos repoName acc =
            case Dict.get repoName repos of
                Just repo ->
                    repo :: acc

                Nothing ->
                    acc
    in
    Pgs.getIds userName model.starred
        |> List.foldr (toRepoList model.repos) []
        |> List.map (\repo -> li [] [ text repo.fullName ])


viewLoadMore : String -> Pgs String -> Html Msg
viewLoadMore userName starred =
    if Pgs.isFetching userName starred then
        div [] [ text "Loading..." ]

    else
        case Pgs.nextPageUrl userName starred of
            Just url ->
                button [ type_ "button", onClick (WantMoreStarred userName url) ]
                    [ text "Load more" ]

            Nothing ->
                span [] []
