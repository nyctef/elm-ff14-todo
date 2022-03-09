module Main exposing (main)

import Browser
import App exposing (..)

main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

