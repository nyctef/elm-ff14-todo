module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, input, label, li, span, text, ul)
import Html.Attributes exposing (checked, type_)
import Html.Events exposing (onCheck)
import Task
import Time
import Time.Extra as Time exposing (Interval(..))
import TimeFormatting exposing (..)


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
    { id : TodoId, name : String, reset : Reset, lastDone : Maybe Time.Posix }


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
            [ { id = TodoId 1, name = "Custom deliveries", reset = weeklyReset, lastDone = Nothing }
            , { id = TodoId 2, name = "Duty roulettes", reset = dailyReset1, lastDone = Nothing }
            , { id = TodoId 3, name = "GC turn-ins", reset = dailyReset2, lastDone = Nothing }
            ]
      }
    , Task.perform SetTz Time.here
    )


type Msg
    = SetTz Time.Zone
    | Tick Time.Posix
    | SetTodoDone TodoId
    | SetTodoUndone TodoId


setTodoState todoId newState =
    List.map
        (\item ->
            if item.id == todoId then
                { item | lastDone = newState }

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
            ( { model | todos = setTodoState id (Just model.instant) model.todos }, Cmd.none )

        SetTodoUndone id ->
            ( { model | todos = setTodoState id Nothing model.todos }, Cmd.none )

isPresent = Maybe.map (\_ -> True) >> Maybe.withDefault False

viewTodo : Time.Posix -> Todo -> Html Msg
viewTodo now todo =
    span []
        [ label []
            [ input
                [ type_ "checkbox"
                , checked (isPresent todo.lastDone)
                , onCheck
                    (\checked ->
                        if checked then
                            SetTodoDone todo.id

                        else
                            SetTodoUndone todo.id
                    )
                ]
                []
            , text todo.name
            ]
        , text "\u{00A0}" -- nbsp
        , text ("Resets in " ++ formatTimeDiff gameTz now (nextReset todo.reset now))
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (formatPosixTime model.myTz model.instant) ]
        , ul [] (model.todos |> List.map (viewTodo model.instant) |> List.map (\x -> li [] [ x ]))
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick
