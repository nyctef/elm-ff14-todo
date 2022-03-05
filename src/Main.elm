module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Task
import Time
import Time.Extra as Time exposing (Interval(..))


gameTz =
    -- ff14's timers are defined in utc
    Time.utc


type alias Reset =
    { interval : Interval, offsetHours : Int }


weeklyReset =
    { interval = Week, offsetHours = 24 + 8 }


dailyReset1 =
    { interval = Day, offsetHours = 15 }


dailyReset2 =
    { interval = Day, offsetHours = 20 }


isLaterThan : Time.Posix -> Time.Posix -> Bool
isLaterThan b a =
    -- note the parameters are backwards from what you might expect!
    -- this is to make partial application work
    Time.posixToMillis a > Time.posixToMillis b


nextReset : Reset -> Time.Posix -> Time.Posix
nextReset reset currentTime =
    let
        startOfCurrentInterval =
            Time.floor reset.interval gameTz currentTime

        startOfNextInterval =
            Time.add reset.interval 1 gameTz startOfCurrentInterval

        resetInCurrentInterval =
            Time.add Hour reset.offsetHours gameTz startOfCurrentInterval

        resetInNextInterval =
            Time.add Hour reset.offsetHours gameTz startOfNextInterval

        result =
            if resetInCurrentInterval |> isLaterThan currentTime then
                resetInCurrentInterval

            else
                resetInNextInterval
    in
    result


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { instant : Time.Posix
    , myTz : Time.Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { instant = Time.millisToPosix 0, myTz = Time.utc }, Task.perform SetTz Time.here )


type Msg
    = SetTz Time.Zone
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTz tz ->
            ( { model | myTz = tz }, Cmd.none )

        Tick instant ->
            ( { model | instant = instant }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.myTz model.instant)

        minute =
            String.fromInt (Time.toMinute model.myTz model.instant)

        second =
            String.fromInt (Time.toSecond model.myTz model.instant)

        nextReset1Hours =
            Time.diff Hour gameTz model.instant (nextReset dailyReset1 model.instant)
    in
    div []
        [ h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
        , text ("Duties reset in " ++ String.fromInt nextReset1Hours ++ " hours")
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick
