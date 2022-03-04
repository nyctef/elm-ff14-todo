module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, h1)
import Html.Events exposing (onClick)
import Time
import Task


gameTz =
    -- ff14's timers are defined in utc
    Time.utc


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
    in
    h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick
