module Main exposing (main)

import Browser
import MapEditor exposing (init, subscriptions, update, view)


main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }
