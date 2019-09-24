module Main exposing (main)

import Browser
import MapEditor exposing (init, subscriptions, update, view)


main =
    Browser.element
        { init = \windowSize -> ( init windowSize, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- TODO:
-- > Validate decoded models.
-- > Create sliders for brush width and brush value.
-- > Improve gradient editor dialog.
-- > Polish UI.
-- > Replace download feature with a "save as" feature.
-- > Embed into desktop application and move save and open buttons into top toolbar.
-- > Load file name into state.
-- > Let user pick file name when saving.
-- > Brush value should change when changing min or max makes it out of bounds.
-- > Show border around brush.
-- > Initial layer state should have a gradient with more than one color stop.
