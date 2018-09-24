module Main exposing (main)

import Api.GitHub as GitHub exposing (StarredList)
import Api.Http exposing (Error, Response(..), showError)
import Browser
import Browser.Navigation as Nav
import Debug
import GhDict exposing (GhDict)
import Html exposing (..)
import Html.Attributes exposing (alt, class, href, size, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Msg exposing (Msg(..))
import Page.Repo
import Page.User
import Paginations as Pgs exposing (Pgs)
import Repo exposing (Repo)
import Repo.FullName as FullName exposing (FullName)
import Route exposing (Route, toRoute)
import Url exposing (Url)
import User exposing (User)


type alias Model =
    { key : Nav.Key
    , route : Route
    , query : String
    , users : GhDict User
    , repos : GhDict Repo
    , starred : Pgs String
    , stargazers : Pgs String
    , errMsg : Maybe String
    }


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- INIT --


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    initPage
        { key = key
        , route = toRoute url
        , query = ""
        , users = GhDict.empty
        , repos = GhDict.empty
        , starred = Pgs.empty
        , stargazers = Pgs.empty
        , errMsg = Nothing
        }


initPage : Model -> ( Model, Cmd Msg )
initPage model =
    case model.route of
        Route.Home ->
            ( { model | query = "" }, Cmd.none )

        Route.User name ->
            Page.User.init name model

        Route.Repo fullName ->
            Page.Repo.init fullName model

        _ ->
            ( model, Cmd.none )



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
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
                    ( { model | users = GhDict.insert user.login user model.users }, Cmd.none )

                Err err ->
                    ( { model | errMsg = Just (showError err) }, Cmd.none )

        StarredListFetched userName result ->
            case result of
                Ok res ->
                    ( updateStarred userName model res, Cmd.none )

                Err err ->
                    ( { model | errMsg = Just (showError err) }, Cmd.none )

        WantMoreStarred userName nextUrl ->
            ( { model | starred = Pgs.startFetch userName model.starred }
            , GitHub.starredMore (StarredListFetched userName) nextUrl
            )

        RepoFetched result ->
            case result of
                Ok (Response _ ( repo, user )) ->
                    ( { model
                        | users = GhDict.insert user.login user model.users
                        , repos = GhDict.insert (Repo.fullName repo) repo model.repos
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | errMsg = Just (showError err) }, Cmd.none )

        StargazersFetched fullName result ->
            case result of
                Ok res ->
                    ( updateStargazers fullName model res, Cmd.none )

                Err err ->
                    ( { model | errMsg = Just (showError err) }, Cmd.none )

        WantMoreStargazers fullName nextUrl ->
            ( { model | stargazers = Pgs.startFetch (FullName.toString fullName) model.stargazers }
            , GitHub.stargazersMore (StargazersFetched fullName) nextUrl
            )


updateStarred : String -> Model -> Response StarredList -> Model
updateStarred userName model (Response res starredList) =
    let
        mergeBoth ( repo, user ) m =
            { m
                | users = GhDict.insert user.login user m.users
                , repos = GhDict.insert (Repo.fullName repo) repo m.repos
            }

        finishFetch m =
            { m
                | starred =
                    Pgs.finishFetch userName
                        ( repoNames, GitHub.nextPageUrl res )
                        m.starred
            }

        repoNames =
            List.map (\( repo, _ ) -> Repo.fullName repo) starredList
    in
    List.foldl mergeBoth model starredList |> finishFetch


updateStargazers : FullName -> Model -> Response (List User) -> Model
updateStargazers fullName model (Response res users) =
    let
        userNames =
            List.map .login users

        mergeUser user m =
            { m | users = GhDict.insert user.login user m.users }

        finishFetch m =
            { m
                | stargazers =
                    Pgs.finishFetch
                        (FullName.toString fullName)
                        ( userNames, GitHub.nextPageUrl res )
                        m.stargazers
            }
    in
    List.foldl mergeUser model users |> finishFetch



-- VIEW --


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Sample - GitHub stars"
    , body =
        [ div []
            [ div [ class "header" ]
                [ h1 [] [ text "Explore GitHub users and Repos" ]
                , viewSearchForm model.query
                , viewErrMsg model.errMsg
                ]
            , hr [] []
            , div [ class "page-root" ]
                [ viewPage model ]
            ]
        ]
    }


viewSearchForm : String -> Html Msg
viewSearchForm query =
    div []
        [ p [] [ text "Type username or repo full name and hit 'Go':" ]
        , form [ onSubmit Search ]
            [ input [ size 45, onInput InputQuery, value query ] []
            , button [ type_ "submit" ] [ text "Go!" ]
            ]
        , nav [ class "header-nav" ]
            [ a [ href "/" ] [ text "top" ]
            , a [ href "/ryym" ] [ text "ryym" ]
            , a [ href "/this/is/invalid/path" ] [ text "404" ]
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
            Page.User.view name model

        Route.Repo fullName ->
            Page.Repo.view fullName model

        Route.NotFound ->
            div []
                [ p [] [ text "Not Found" ] ]
