module Msg exposing (Msg(..))

import Api.GitHub exposing (StarredList)
import Api.Http exposing (Error, Response(..))
import Browser
import Repo exposing (Repo)
import Repo.FullName exposing (FullName)
import Url exposing (Url)
import User exposing (User, UserName)


{-| Msg is shared among pages.
-}
type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | InputQuery String
    | Search
    | UserFetched (Result Error (Response User))
    | StarredListFetched UserName (Result Error (Response StarredList))
    | WantMoreStarred UserName String
    | RepoFetched (Result Error (Response ( Repo, User )))
    | StargazersFetched FullName (Result Error (Response (List User)))
    | WantMoreStargazers FullName String
