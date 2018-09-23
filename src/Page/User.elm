module Page.User exposing (init, view)

import Api.GitHub as GitHub
import GhDict exposing (GhDict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msg exposing (Msg(..))
import Page.Part exposing (viewLoadMore)
import Paginations as Pgs exposing (Pgs)
import Repo exposing (Repo)
import User exposing (User)


type alias Model m =
    { m
        | users : GhDict User
        , repos : GhDict Repo
        , starred : Pgs String
        , query : String
    }


init : String -> Model m -> ( Model m, Cmd Msg )
init name model =
    let
        fetchUser =
            if GhDict.member name model.users then
                Cmd.none

            else
                GitHub.user UserFetched name

        fetchStarred =
            if GhDict.member name model.starred then
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


view : String -> Model m -> Html Msg
view name model =
    case GhDict.get name model.users of
        Just user ->
            div []
                [ h2 [] [ text user.login ]
                , img [ src user.avatar.url, alt "", class "user-avatar" ] []
                , h3 [] [ text "starred repositories" ]
                , viewStarred name model
                ]

        Nothing ->
            div []
                [ p [] [ text "Loading..." ] ]


viewStarred : String -> Model m -> Html Msg
viewStarred userName model =
    div []
        [ ul [] <| viewStarredList userName model
        , viewLoadMore (WantMoreStarred userName) userName model.starred
        ]


viewStarredList : String -> Model m -> List (Html Msg)
viewStarredList userName model =
    Pgs.getIds userName model.starred
        |> List.foldr (toRepoList model.repos) []
        |> List.map (\repo -> li [] [ text repo.fullName ])


toRepoList : GhDict Repo -> String -> List Repo -> List Repo
toRepoList repos repoName acc =
    case GhDict.get repoName repos of
        Just repo ->
            repo :: acc

        Nothing ->
            acc
