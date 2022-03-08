module TimeFormattingTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time exposing (Month(..), utc)
import Time.Extra exposing (Parts, partsToPosix)
import TimeFormatting exposing (..)


suite =
    describe "time formatting"
        [ test "times are formatted with leading zeros" <|
            \_ -> Expect.equal "01:02:03" (formatPosixTime utc <| partsToPosix utc <| Parts 2020 Jan 20 1 2 3 99)
        , test "time diffs are formatted with leading zeros" <|
            \_ ->
                let
                    time1 =
                        partsToPosix utc <| Parts 2020 Jan 20 1 2 3 99

                    time2 =
                        partsToPosix utc <| Parts 2020 Jan 20 4 4 4 99
                in
                Expect.equal "03:02:01" (formatTimeDiff utc time1 time2)
        ]
