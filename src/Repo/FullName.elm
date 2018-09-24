module Repo.FullName exposing (FullName(..), decoder, fromString, toString)

import Json.Decode as J exposing (Decoder)


type alias OwnerName =
    String


type FullName
    = FullName OwnerName String


fromString : String -> Maybe FullName
fromString s =
    case String.split "/" s of
        [ owner, name ] ->
            Just <| FullName owner name

        _ ->
            Nothing


toString : FullName -> String
toString (FullName owner name) =
    owner ++ "/" ++ name


decoder : String -> Decoder FullName
decoder s =
    case fromString s of
        Just name ->
            J.succeed name

        Nothing ->
            J.fail <| "invalid full name: " ++ s
