module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, label, li, span, text, ul)
import Html.Attributes exposing (checked, type_)
import Html.Events exposing (onCheck, onClick)
import String exposing (padLeft)
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


type TodoId
    = TodoId Int


type alias Todo =
    { id : TodoId, name : String, reset : Reset, done : Bool }


type alias Model =
    { instant : Time.Posix
    , myTz : Time.Zone
    , todos : List Todo
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { instant = Time.millisToPosix 0
      , myTz = Time.utc
      , todos =
            [ { id = TodoId 1, name = "Custom deliveries", reset = weeklyReset, done = False }
            , { id = TodoId 2, name = "Duty roulettes", reset = dailyReset1, done = False }
            , { id = TodoId 3, name = "GC turn-ins", reset = dailyReset2, done = False }
            ]
      }
    , Task.perform SetTz Time.here
    )


type Msg
    = SetTz Time.Zone
    | Tick Time.Posix
    | SetTodoDone TodoId
    | SetTodoUndone TodoId


setTodoState todoId done =
    List.map
        (\item ->
            if item.id == todoId then
                { item | done = done }

            else
                item
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTz tz ->
            ( { model | myTz = tz }, Cmd.none )

        Tick instant ->
            ( { model | instant = instant }, Cmd.none )

        SetTodoDone id ->
            ( { model | todos = setTodoState id True model.todos }, Cmd.none )

        SetTodoUndone id ->
            ( { model | todos = setTodoState id False model.todos }, Cmd.none )


formatTimePart =
    String.fromInt >> padLeft 2 '0'


viewPosixTimeText : Time.Zone -> Time.Posix -> String
viewPosixTimeText tz time =
    let
        hour =
            Time.toHour tz time

        minute =
            Time.toMinute tz time

        second =
            Time.toSecond tz time
    in
    (hour |> formatTimePart) ++ ":" ++ (minute |> formatTimePart) ++ ":" ++ (second |> formatTimePart)


formatTimeDiff hourDiff minDiff secondDiff =
    (hourDiff |> formatTimePart) ++ ":" ++ (minDiff |> modBy 60 |> formatTimePart) ++ ":" ++ (secondDiff |> modBy 60 |> formatTimePart)


viewDiffTimeText : Time.Zone -> Time.Posix -> Time.Posix -> String
viewDiffTimeText tz time1 time2 =
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
    formatTimeDiff hourDiff minDiff secondDiff


viewTodo : Time.Posix -> Todo -> Html Msg
viewTodo now todo =
    span []
        [ input
            [ type_ "checkbox"
            , checked todo.done
            , onCheck
                (\checked ->
                    if checked then
                        SetTodoDone todo.id

                    else
                        SetTodoUndone todo.id
                )
            ]
            []
        , label [] [ text todo.name ]
        , text "\u{00A0}" -- nbsp
        , text ("Resets in " ++ viewDiffTimeText gameTz now (nextReset todo.reset now))
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (viewPosixTimeText model.myTz model.instant) ]
        , ul [] (model.todos |> List.map (viewTodo model.instant) |> List.map (\x -> li [] [ x ]))
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick
