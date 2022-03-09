module AppTest exposing (..)

import App exposing (Msg(..), init, update)
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


suite =
    describe "update function"
        [ test "setting the initial model" <|
            let
                now =
                    partsToPosix utc <| Parts 2020 Jan 20 1 2 3 99

                model =
                    dumbInit () |> dumbUpdate (SetTz utc) |> dumbUpdate (Tick now)
            in
            \_ ->
                Expect.all
                    [ .myTz >> Expect.equal utc
                    , .instant >> Expect.equal now
                    ]
                    model
        ]
