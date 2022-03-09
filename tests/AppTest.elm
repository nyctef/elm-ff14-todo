module AppTest exposing (..)

import App exposing (update, init, Msg(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time exposing (utc)

-- throw away commands since we can't handle them in tests
dumbInit = init >> Tuple.first
dumbUpdate x = update x >> Tuple.first

suite =
    describe "update function"
        [ test "setting the initial model" <|
            let
                model = dumbInit () |> dumbUpdate (SetTz utc) 
            in
            \_ -> Expect.equal utc model.myTz
        ]
