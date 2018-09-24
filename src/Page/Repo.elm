module Page.Repo exposing (init, view)

import Api.GitHub as GitHub
import GhDict exposing (GhDict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msg exposing (Msg(..))
import Page.Part exposing (viewLoadMore)
import Paginations as Pgs exposing (Pgs)
import Repo exposing (Repo)
import Repo.FullName as FullName exposing (FullName)
import User exposing (User)


type alias Model m =
    { m
        | users : GhDict User
        , repos : GhDict Repo
        , stargazers : Pgs String
        , query : String
    }


init : FullName -> Model m -> ( Model m, Cmd Msg )
init fullName_ model =
    let
        fullName =
            FullName.toString fullName_

        fetchRepo =
            if GhDict.member fullName model.repos then
                Cmd.none

            else
                GitHub.repository RepoFetched fullName

        hasFetchedStargazers =
            GhDict.member fullName model.stargazers
    in
    if hasFetchedStargazers then
        ( { model | query = fullName }, fetchRepo )

    else
        ( { model
            | stargazers = Pgs.startFetch fullName model.stargazers
            , query = fullName
          }
        , Cmd.batch
            [ fetchRepo
            , GitHub.stargazers (StargazersFetched fullName) fullName
            ]
        )


view : FullName -> Model m -> Html Msg
view fullName_ model =
    let
        fullName =
            FullName.toString fullName_
    in
    case GhDict.get fullName model.repos of
        Just repo ->
            div []
                [ a [ href repo.htmlUrl, target "_blank" ] [ h2 [] [ text fullName ] ]
                , p [] [ text <| Maybe.withDefault "no description" repo.description ]
                , h3 [] [ text "stargazers" ]
                , viewStargazers fullName model
                ]

        Nothing ->
            div []
                [ p [] [ text "Loading.." ] ]


viewStargazers : String -> Model m -> Html Msg
viewStargazers fullName model =
    div []
        [ ul [ class "user-list" ] <| viewStargazerList fullName model
        , viewLoadMore (WantMoreStargazers fullName) fullName model.stargazers
        ]


viewStargazerList : String -> Model m -> List (Html Msg)
viewStargazerList fullName model =
    Pgs.getIds fullName model.stargazers
        |> List.foldr (toUserList model.users) []
        |> List.map viewUserItem


toUserList users userName acc =
    case GhDict.get userName users of
        Just user ->
            user :: acc

        Nothing ->
            acc


viewUserItem : User -> Html Msg
viewUserItem user =
    li [ class "user-item" ]
        [ a [ href ("/" ++ user.login) ]
            [ img [ class "user-avatar", src user.avatar.url ] []
            , div [ class "user-login" ] [ text user.login ]
            ]
        ]
