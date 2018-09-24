module TestPagination exposing (suite)

import Expect
import Pagination as Pg
import Test exposing (..)


suite : Test
suite =
    describe "Pagination"
        [ describe ".startFetch"
            [ test "turns isFetching on" <|
                \_ ->
                    let
                        empty =
                            Pg.empty |> (\m -> { m | isFetching = False })
                    in
                    Pg.startFetch empty
                        |> .isFetching
                        |> Expect.equal True
            ]
        , describe ".finishFetch"
            [ test "updates pagination state" <|
                \_ ->
                    let
                        from =
                            { isFetching = True
                            , nextPageUrl = Just "page?=6"
                            , pageCount = 5
                            , ids = [ 1, 2, 3 ]
                            }
                    in
                    Pg.finishFetch [ 4, 5, 6 ] Nothing from
                        |> Expect.equal
                            { isFetching = False
                            , nextPageUrl = Nothing
                            , pageCount = 6
                            , ids = [ 1, 2, 3, 4, 5, 6 ]
                            }
            ]
        ]
