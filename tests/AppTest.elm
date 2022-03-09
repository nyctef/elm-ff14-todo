module AppTest exposing (..)

import App exposing (Model, Msg(..), TodoId(..), init, update)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time exposing (Month(..), utc)
import Time.Extra exposing (Parts, partsToPosix)


dumbInit =
    -- throw away commands since we can't easily handle them in tests
    init >> Tuple.first


dumbUpdate x =
    -- throw away commands since we can't easily handle them in tests
    update x >> Tuple.first


updates : List Msg -> Model
updates =
    List.foldl dumbUpdate (dumbInit ())


todoWithId id =
    List.filter (\x -> x.id == TodoId id) >> List.head


fail : String -> Expectation
fail message =
    Expect.true message False


expectJust : (a -> Expectation) -> Maybe a -> Expectation
expectJust y x =
    x |> Maybe.map y |> Maybe.withDefault (fail "Expected Just, but got Nothing")


suite =
    describe "update function"
        [ test "setting the initial model" <|
            let
                now =
                    partsToPosix utc <| Parts 2020 Jan 20 1 2 3 99
            in
            \_ ->
                updates [ SetTz utc, Tick now ]
                    |> Expect.all
                        [ .myTz >> Expect.equal utc
                        , .instant >> Expect.equal now
                        ]
        , test "setting a todo done" <|
            let
                now =
                    partsToPosix utc <| Parts 2020 Jan 20 1 2 3 99
            in
            \_ ->
                updates [ SetTz utc, Tick now, SetTodoDone (TodoId 1) ]
                    |> .todos
                    |> todoWithId 1
                    |> expectJust
                        (Expect.all
                            [ .id >> Expect.equal (TodoId 1)
                            , .lastDone >> Expect.equal (Just now)
                            ]
                        )
        , test "resetting a todo" <|
            let
                todayAt2pm =
                    partsToPosix utc <| Parts 2020 Jan 20 14 0 0 0

                todayAfter3pm =
                    partsToPosix utc <| Parts 2020 Jan 20 15 1 0 0
            in
            \_ ->
                updates [ SetTz utc, Tick todayAt2pm, SetTodoDone (TodoId 2), Tick todayAfter3pm ]
                    |> .todos
                    |> todoWithId 2
                    |> expectJust
                        (Expect.all
                            [ .lastDone >> Expect.equal Nothing
                            ]
                        )
        ]
