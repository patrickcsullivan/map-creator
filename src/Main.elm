module Main exposing (main)

import Browser
import Model
import Update exposing (update)
import View exposing (view)


main =
    Browser.sandbox
        { init = Model.init
        , update = update
        , view = view
        }
