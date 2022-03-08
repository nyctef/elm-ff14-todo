module TimeFormatting exposing (formatPosixTime, formatTimeDiff)

import String exposing (padLeft)
import Time
import Time.Extra as Time exposing (Interval(..))


formatTimePart =
    String.fromInt >> padLeft 2 '0'


formatPosixTime : Time.Zone -> Time.Posix -> String
formatPosixTime tz time =
    let
        hour =
            Time.toHour tz time

        minute =
            Time.toMinute tz time

        second =
            Time.toSecond tz time
    in
    (hour |> formatTimePart) ++ ":" ++ (minute |> formatTimePart) ++ ":" ++ (second |> formatTimePart)


formatTimeDiff_ hourDiff minDiff secondDiff =
    (hourDiff |> formatTimePart) ++ ":" ++ (minDiff |> modBy 60 |> formatTimePart) ++ ":" ++ (secondDiff |> modBy 60 |> formatTimePart)


formatTimeDiff : Time.Zone -> Time.Posix -> Time.Posix -> String
formatTimeDiff tz time1 time2 =
    -- note: currently the passed-in tz parameter doesn't matter here,
    -- since it's only used by Time.diff for figuring out what day it should be.
    --
    -- however, we really should include number of days different in this string:
    -- and since a day doesn't always last 24 hours, in theory we need tz data to
    -- correctly determine whether two instants are a day apart or not.
    --
    -- however, the tz data provided by Time.here only gives us a fixed utc offset:
    -- https://package.elm-lang.org/packages/elm/time/latest/Time#here
    -- so we'd still have the bug in practice until elm is able to give better tz data.
    let
        hourDiff =
            Time.diff Hour tz time1 time2

        minDiff =
            Time.diff Minute tz time1 time2

        secondDiff =
            Time.diff Second tz time1 time2
    in
    formatTimeDiff_ hourDiff minDiff secondDiff
