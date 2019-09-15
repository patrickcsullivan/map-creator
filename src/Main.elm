module Main exposing (main)

import Browser
import MapEditor exposing (init, update, view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : MapEditor.State -> Sub MapEditor.Msg
subscriptions _ =
    Sub.none
