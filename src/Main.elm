module Main exposing (main)

import Browser
import MapEditor exposing (init, subscriptions, update, view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
