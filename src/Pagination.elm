module Pagination exposing (Pg, empty, finishFetch, startFetch)


type alias Pg v =
    { isFetching : Bool
    , nextPageUrl : Maybe String
    , pageCount : Int
    , ids : List v
    }


empty : Pg v
empty =
    { isFetching = False
    , nextPageUrl = Nothing
    , pageCount = 0
    , ids = []
    }


startFetch : Pg v -> Pg v
startFetch pg =
    { pg | isFetching = True }


finishFetch : List v -> Maybe String -> Pg v -> Pg v
finishFetch ids url pg =
    { pg
        | isFetching = False
        , nextPageUrl = url
        , pageCount = pg.pageCount + 1
        , ids = List.append pg.ids ids
    }
