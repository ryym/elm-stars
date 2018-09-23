module Msg exposing (Msg(..))

import Api.GitHub exposing (StarredList)
import Api.Http exposing (Error, Response(..))
import Browser
import Repo exposing (Repo)
import Url exposing (Url)
import User exposing (User)


{-| Msg is shared among pages.
-}
type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | InputQuery String
    | Search
    | UserFetched (Result Error (Response User))
    | StarredListFetched String (Result Error (Response StarredList))
    | WantMoreStarred String String
    | RepoFetched (Result Error (Response ( Repo, User )))
    | StargazersFetched String (Result Error (Response (List User)))
    | WantMoreStargazers String String
