module Repo.WithOwner exposing (RepoWithOwner, decoder)

import Json.Decode as J exposing (Decoder, field)
import Repo
import User


type alias RepoWithOwner =
    ( Repo, User )


decoder : Decoder RepoWithOwner
decoder =
    J.map2 (\repo owner -> ( repo, owner ))
        Repo.decoder
        (field "owner" User.decoder)
