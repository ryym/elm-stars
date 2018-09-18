module Route exposing (Route(..), toRoute)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, string, top)


type Route
    = Home
    | User String
    | NotFound


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map User (top </> string)
        ]


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse route url)
