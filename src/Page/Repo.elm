module Page.Repo exposing (init, view)

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
        , stargazers : Pgs String
        , query : String
    }


init : String -> Model m -> ( Model m, Cmd Msg )
init fullName model =
    let
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


view : String -> Model m -> Html Msg
view fullName model =
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


viewStargazers : String -> Model m -> Html Msg
viewStargazers fullName model =
    div []
        [ ul [] <| viewStargazerList fullName model
        , viewLoadMore (WantMoreStargazers fullName) fullName model.stargazers
        ]


viewStargazerList : String -> Model m -> List (Html Msg)
viewStargazerList fullName model =
    Pgs.getIds fullName model.stargazers
        |> List.foldr (toUserList model.users) []
        |> List.map (\user -> li [] [ text user.login ])


toUserList users userName acc =
    case GhDict.get userName users of
        Just user ->
            user :: acc

        Nothing ->
            acc
