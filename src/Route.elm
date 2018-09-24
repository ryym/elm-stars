module Route exposing (Route(..), toRoute)

import Repo.FullName as FullName exposing (FullName(..))
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, string, top)


type Route
    = Home
    | User String
    | Repo FullName
    | NotFound


repoRoute owner name =
    Repo <| FullName owner name


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map User (top </> string)
        , map repoRoute (top </> string </> string)
        ]


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse route url)
