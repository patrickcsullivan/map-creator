module Main exposing (main)

import Browser
import MapEditor exposing (init, update, view)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
