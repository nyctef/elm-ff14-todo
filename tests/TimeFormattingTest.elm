module TimeFormattingTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time exposing (utc, Month(..))
import TimeFormatting exposing (..)
import Time.Extra exposing (Parts, partsToPosix)


suite : Test
suite =
    test "times are formatted with leading zeros"
        (\_ -> Expect.equal "01:02:03" (formatPosixTime utc (partsToPosix utc (Parts 2020 Jan 20 1 2 3 99))))
