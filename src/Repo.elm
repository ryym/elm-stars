module Repo exposing (Repo, decoder, fullName)

import Json.Decode as J exposing (Decoder, field)
import Repo.FullName as FullName exposing (FullName(..))


type alias Repo =
    { name : String
    , fullName : FullName
    , description : Maybe String
    , htmlUrl : String
    , ownerName : String
    }


decoder =
    J.map5 Repo
        (field "name" J.string)
        (field "full_name" J.string |> J.andThen FullName.decoder)
        (field "description" (J.nullable J.string))
        (field "html_url" J.string)
        (J.at [ "owner", "login" ] J.string)


fullName : Repo -> String
fullName repo =
    FullName.toString repo.fullName
