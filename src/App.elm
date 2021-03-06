module App exposing (Model, Msg(..), Reset, Todo, TodoId(..), init, subscriptions, update, view)

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


isLaterThan : Time.Posix -> Time.Posix -> Bool
isLaterThan b a =
    -- note the parameters are backwards from what you might expect!
    -- this is to make partial application work
    Time.posixToMillis a > Time.posixToMillis b


isLaterThanMaybe : Maybe Time.Posix -> Time.Posix -> Bool
isLaterThanMaybe b a =
    Maybe.map (\b_ -> isLaterThan b_ a) b |> Maybe.withDefault False


{-| Imagine a timeline like the following:

    --|----R----|----R----|
    --  ^a   ^b



where `|` indicates the interval (day/week) boundaries
and R indicates the time of the reset within that interval.

The current time is either a or b: in our current interval, we're
either before or after the reset in that interval.
In case a, we just use the reset in the current interval:
otherwise we jump ahead one interval and use that reset as
next one instead.

-}
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


{-| same as `nextReset`, but with this timeline instead:

    --|----R----|----R----|
    --            ^a   ^b



and searching backwards instead of forwards.

-}
prevReset : Reset -> Time.Posix -> Time.Posix
prevReset reset currentTime =
    let
        startOfCurrentInterval =
            Time.floor reset.interval gameTz currentTime

        startOfPrevInterval =
            Time.add reset.interval -1 gameTz startOfCurrentInterval

        resetInCurrentInterval =
            Time.add Hour reset.offsetHours gameTz startOfCurrentInterval

        resetInPrevInterval =
            Time.add Hour reset.offsetHours gameTz startOfPrevInterval

        result =
            if currentTime |> isLaterThan resetInCurrentInterval then
                resetInCurrentInterval

            else
                resetInPrevInterval
    in
    result


type TodoId
    = TodoId String


type alias Todo =
    { id : TodoId, name : String, reset : Reset, lastDone : Maybe Time.Posix }


type alias Model =
    { instant : Time.Posix
    , myTz : Time.Zone
    , todos : List Todo
    }


init : List Todo -> ( Model, Cmd Msg )
init initialTodos =
    ( { instant = Time.millisToPosix 0
      , myTz = Time.utc
      , todos = initialTodos
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


resetTodos : Time.Posix -> List Todo -> List Todo
resetTodos now =
    List.map
        (\item ->
            let
                reset =
                    prevReset item.reset now

                newState =
                    if reset |> isLaterThanMaybe item.lastDone then
                        Nothing

                    else
                        item.lastDone
            in
            { item | lastDone = newState }
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTz tz ->
            ( { model | myTz = tz }, Cmd.none )

        Tick instant ->
            ( { model | instant = instant, todos = resetTodos instant model.todos }, Cmd.none )

        SetTodoDone id ->
            ( { model | todos = setTodoState id (Just model.instant) model.todos }, Cmd.none )

        SetTodoUndone id ->
            ( { model | todos = setTodoState id Nothing model.todos }, Cmd.none )


isPresent =
    Maybe.map (\_ -> True) >> Maybe.withDefault False


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


sortTodos : Time.Posix -> List Todo -> List Todo
sortTodos now =
    List.sortBy (\x -> Time.posixToMillis (nextReset x.reset now))


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (formatPosixTime model.myTz model.instant) ]
        , ul [] (model.todos |> sortTodos model.instant |> List.map (viewTodo model.instant) |> List.map (\x -> li [] [ x ]))
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick
