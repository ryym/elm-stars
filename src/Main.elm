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
import Page.User
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

        Route.Repo owner name ->
            let
                fullName =
                    owner ++ "/" ++ name

                fetchRepo =
                    if GhDict.member fullName model.repos then
                        Cmd.none

                    else
                        GitHub.repository RepoFetched fullName

                fetchStargazers =
                    if GhDict.member fullName model.stargazers then
                        Cmd.none

                    else
                        GitHub.stargazers (StargazersFetched fullName) fullName
            in
            ( { model
                | query = fullName
              }
            , Cmd.batch [ fetchRepo, fetchStargazers ]
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
                    ( { model | users = GhDict.insert user.login user model.users }, Cmd.none )

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
                                | users = GhDict.insert user.login user m.users
                                , repos = GhDict.insert repo.fullName repo m.repos
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

        RepoFetched result ->
            case result of
                Ok (Response _ ( repo, user )) ->
                    ( { model
                        | users = GhDict.insert user.login user model.users
                        , repos = GhDict.insert repo.fullName repo model.repos
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | errMsg = Just (showError err) }, Cmd.none )

        StargazersFetched fullName result ->
            case result of
                Ok (Response res users) ->
                    let
                        reduce m =
                            List.foldl mergeUser m users |> finishFetch

                        userNames =
                            List.map .login users

                        mergeUser user m =
                            { m | users = GhDict.insert user.login user m.users }

                        finishFetch m =
                            { m | stargazers = Pgs.finishFetch fullName ( userNames, nextPageUrl ) m.stargazers }

                        nextPageUrl =
                            GitHub.nextPageUrl res
                    in
                    ( reduce model, Cmd.none )

                Err err ->
                    ( { model | errMsg = Just (showError err) }, Cmd.none )

        WantMoreStargazers fullName nextUrl ->
            ( { model | stargazers = Pgs.startFetch fullName model.stargazers }
            , GitHub.stargazersMore (StargazersFetched fullName) nextUrl
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
            Page.User.view name model

        Route.Repo owner name ->
            let
                fullName =
                    owner ++ "/" ++ name
            in
            case GhDict.get fullName model.repos of
                Just repo ->
                    div []
                        [ h2 [] [ text repo.fullName ]
                        , p [] [ text <| Maybe.withDefault "" repo.description ]
                        , h3 [] [ text "stargazers" ]
                        , viewStargazers fullName model
                        ]

                Nothing ->
                    div []
                        [ p [] [ text "Loading.." ] ]

        Route.NotFound ->
            div []
                [ p [] [ text "Not Found" ] ]


viewLoadMore : (String -> Msg) -> String -> Pgs v -> Html Msg
viewLoadMore msg key pgs =
    if Pgs.isFetching key pgs then
        div [] [ text "Loading..." ]

    else
        case Pgs.nextPageUrl key pgs of
            Just url ->
                button [ type_ "button", onClick (msg url) ]
                    [ text "Load more" ]

            Nothing ->
                span [] []


viewStargazers : String -> Model -> Html Msg
viewStargazers fullName model =
    div []
        [ ul [] <| viewStargazerList fullName model
        , viewLoadMore (WantMoreStargazers fullName) fullName model.stargazers
        ]


viewStargazerList : String -> Model -> List (Html Msg)
viewStargazerList fullName model =
    let
        toUserList users userName acc =
            case GhDict.get userName users of
                Just user ->
                    user :: acc

                Nothing ->
                    acc
    in
    Pgs.getIds fullName model.stargazers
        |> List.foldr (toUserList model.users) []
        |> List.map (\user -> li [] [ text user.login ])
