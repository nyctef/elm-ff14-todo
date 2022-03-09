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
                    |> Maybe.map (Expect.all [ .id >> Expect.equal (TodoId 1), .lastDone >> Expect.equal (Just now) ])
                    |> Maybe.withDefault (Expect.true "todo not found" False)
        ]