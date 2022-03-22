port module Main exposing (main)

import App exposing (..)
import Browser
import Iso8601
import Json.Decode as D
import Json.Encode as E
import Time.Extra exposing (Interval(..))



-- we only use a port for setting storage after each update:
-- reading the stored values happens at startup via elm flags


port setStorage : E.Value -> Cmd msg


encodeInterval : Interval -> E.Value
encodeInterval i =
    E.string
        (case i of
            Week ->
                "week"

            Day ->
                "day"

            _ ->
                -- we're not expecting any other intervals here for the moment
                "error"
        )


encodeReset : Reset -> E.Value
encodeReset reset =
    E.object
        [ ( "interval", encodeInterval reset.interval )
        , ( "offsetHours", E.int reset.offsetHours )
        ]


extractTodoId (TodoId withId) =
    withId


encodeTodo : Todo -> E.Value
encodeTodo todo =
    E.object
        [ ( "id", E.string <| extractTodoId todo.id )
        , ( "name", E.string todo.name )
        , ( "reset", encodeReset todo.reset )
        , ( "lastDone"
          , case todo.lastDone of
                Just time ->
                    E.string <| Iso8601.fromTime time

                Nothing ->
                    E.null
          )
        ]


encode : Model -> E.Value
encode model =
    E.list encodeTodo model.todos


resetDecoder : D.Decoder Reset
resetDecoder =
    D.map2 Reset
        (D.field "interval" D.string |> D.andThen parseInterval)
        (D.field "offsetHours" D.int)


parseInterval : String -> D.Decoder Interval
parseInterval x =
    case x of
        "week" ->
            D.succeed Week

        "day" ->
            D.succeed Day

        _ ->
            D.fail "only day / week supported for now"


todoDecoder : D.Decoder Todo
todoDecoder =
    D.map4 Todo
        (D.field "id" (D.map TodoId D.string))
        (D.field "name" D.string)
        (D.field "reset" resetDecoder)
        (D.field "lastDone" (D.nullable Iso8601.decoder))


decoder : D.Decoder (List Todo)
decoder =
    D.list todoDecoder


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ setStorage (encode newModel), cmds ]
    )


weeklyReset =
    { interval = Week, offsetHours = 24 + 8 }


dailyReset1 =
    { interval = Day, offsetHours = 15 }


dailyReset2 =
    { interval = Day, offsetHours = 20 }

lotteryReset = 
    -- todo: doublecheck the math on this one
    { interval = Week, offsetHours = (5 * 24) + 20}


defaultTodos =
    [ { id = TodoId "custom-deliveries", name = "Custom deliveries", reset = weeklyReset, lastDone = Nothing }
    , { id = TodoId "yorha", name = "YoRHa weekly quest", reset = weeklyReset, lastDone = Nothing }
    , { id = TodoId "astronomy", name = "Astronomy tomestone cap", reset = weeklyReset, lastDone = Nothing }
    , { id = TodoId "wondrous-tails-1", name = "Pick up Wondrous Tails", reset = weeklyReset, lastDone = Nothing }
    , { id = TodoId "wondrous-tails-2", name = "Complete Wondrous Tails", reset = weeklyReset, lastDone = Nothing }
    , { id = TodoId "gc-elite-1", name = "Pick up GC elite hunting mark", reset = weeklyReset, lastDone = Nothing }
    , { id = TodoId "gc-elite-2", name = "Complete GC elite hunting mark", reset = weeklyReset, lastDone = Nothing }
    , { id = TodoId "ish-elite-1", name = "Pick up Ishgard elite hunting mark", reset = weeklyReset, lastDone = Nothing }
    , { id = TodoId "ish-elite-2", name = "Complete Ishgard elite hunting mark", reset = weeklyReset, lastDone = Nothing }
    , { id = TodoId "sb-elite-1", name = "Pick up Stormblood elite hunting mark", reset = weeklyReset, lastDone = Nothing }
    , { id = TodoId "sb-elite-2", name = "Complete Stormblood elite hunting mark", reset = weeklyReset, lastDone = Nothing }
    , { id = TodoId "doman", name = "Doman enclave donations", reset = weeklyReset, lastDone = Nothing }
    , { id = TodoId "duty-roulettes", name = "Duty roulettes", reset = dailyReset1, lastDone = Nothing }
    , { id = TodoId "beast tribes", name = "Beast tribe daily quests", reset = dailyReset1, lastDone = Nothing }
    , { id = TodoId "mini-cactpot", name = "Mini Cactpot", reset = dailyReset1, lastDone = Nothing }
    , { id = TodoId "jumbo-cactpot", name = "Jumbo Cactpot", reset = lotteryReset, lastDone = Nothing }
    , { id = TodoId "gc-turnins", name = "GC turn-ins", reset = dailyReset2, lastDone = Nothing }
    ]


initWithStorage : E.Value -> ( Model, Cmd Msg )
initWithStorage flags =
    case D.decodeValue decoder flags of
        Ok initialTodos ->
            if List.isEmpty initialTodos then
                init defaultTodos

            else
                init initialTodos

        Err _ ->
            init defaultTodos


main =
    Browser.element { init = initWithStorage, update = updateWithStorage, view = view, subscriptions = subscriptions }
