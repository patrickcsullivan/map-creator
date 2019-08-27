module Model exposing
    ( Model
    , init
    )

import Brush exposing (Brush)
import Layer exposing (Layer)



-- INVARIANTS:
-- Width must be greater than 0.
-- Height must be greater than 0.
-- All layers must have same width and same height.
-- If selected layer index exists it must be greater than 0 and less than the number of layers.


type alias Model =
    { name : String
    , width : Int
    , height : Int
    , layers : List Layer
    , selectedLayerAndTool : Maybe ( Int, Tool )
    }


type Tool
    = Pan
    | Brush Brush


init : Model
init =
    { name = "untitled"
    , width = 50
    , height = 50
    , layers = []
    , selectedLayerAndTool = Nothing
    }



-- TODO:
-- getName : Model -> String
-- getWidth : Model -> Int
-- getHeight : Model -> Int
-- getLayers : Model -> List Layer
-- getSelectedLayerIndex : Model -> Maybe Int
-- getSelectedLayer : Model -> Maybe Layer
-- selectLayer : Int -> Model -> Model
-- deleteLayer : Int -> Model -> Model
-- newLayer : String -> Int -> Int -> Model
-- resize : Int -> Int -> Model
-- getTool : Model -> Tool
-- selectBrushTool : Model -> Model
-- selectPanTool : Model -> Model
-- stroke stuff
