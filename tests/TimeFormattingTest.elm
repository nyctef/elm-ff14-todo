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
        , test "time diffs greater than one day are shown like 5d6h" <|
            \_ ->
                let
                    time1 =
                        partsToPosix utc <| Parts 2020 Jan 20 1 2 3 99

                    time2 =
                        partsToPosix utc <| Parts 2020 Jan 22 4 4 4 99
                in
                Expect.equal "2d3h" (formatTimeDiff utc time1 time2)
        , test "time diffs less than one day are shown like 5h6m" <|
            \_ ->
                let
                    time1 =
                        partsToPosix utc <| Parts 2020 Jan 22 1 2 3 99

                    time2 =
                        partsToPosix utc <| Parts 2020 Jan 22 4 4 4 99
                in
                Expect.equal "3h2m" (formatTimeDiff utc time1 time2)
        , test "time diffs less than one minute are just shown as seconds" <|
            \_ ->
                let
                    time1 =
                        partsToPosix utc <| Parts 2020 Jan 22 4 4 0 99

                    time2 =
                        partsToPosix utc <| Parts 2020 Jan 22 4 4 20 99
                in
                Expect.equal "20s" (formatTimeDiff utc time1 time2)
        ]
