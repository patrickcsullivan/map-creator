module Model exposing
    ( Modal(..)
    , Model
    , Tool
    , closeModal
    , createLayer
    , deleteSelectedLayer
    , getHeight
    , getLayers
    , getModal
    , getName
    , getSelectedLayerAndTool
    , getWidth
    , init
    , openNewLayerDialog
    , selectLayer
    , setNewLayerDialogName
    , setSelectedLayerMax
    , setSelectedLayerMin
    )

import Brush exposing (Brush)
import Layer exposing (Layer)
import List.Extra exposing (getAt)



-- INVARIANTS:
-- Width must be greater than 0.
-- Height must be greater than 0.
-- All layers must have same width and same height.
-- If selected layer index exists it must be greater than 0 and less than the number of layers.


type Model
    = Model InnerModel


type alias InnerModel =
    { name : String
    , width : Int
    , height : Int
    , layers : List Layer
    , selectedLayerIndexAndTool : Maybe ( Int, Tool )
    , modal : Maybe Modal
    }


type Tool
    = Pan
    | Brush Brush


type Modal
    = NewLayerDialog String


init : Model
init =
    Model
        { name = "untitled"
        , width = 10
        , height = 10
        , layers = []
        , selectedLayerIndexAndTool = Nothing
        , modal = Nothing
        }


getName : Model -> String
getName (Model inner) =
    inner.name


getWidth : Model -> Int
getWidth (Model inner) =
    inner.width


getHeight : Model -> Int
getHeight (Model inner) =
    inner.height


getLayers : Model -> List Layer
getLayers (Model inner) =
    inner.layers


getSelectedLayerAndTool : Model -> Maybe ( Int, Layer, Tool )
getSelectedLayerAndTool (Model inner) =
    case inner.selectedLayerIndexAndTool of
        Nothing ->
            Nothing

        Just ( layerIndex, tool ) ->
            case getAt layerIndex inner.layers of
                Nothing ->
                    Nothing

                Just layer ->
                    Just ( layerIndex, layer, tool )



-- MODAL / DIALOGS


getModal : Model -> Maybe Modal
getModal (Model inner) =
    inner.modal


openNewLayerDialog : Model -> Model
openNewLayerDialog (Model inner) =
    Model
        { inner
            | modal = Just (NewLayerDialog "New Layer")
        }


setNewLayerDialogName : String -> Model -> Model
setNewLayerDialogName layerName (Model inner) =
    case inner.modal of
        Just (NewLayerDialog _) ->
            Model
                { inner
                    | modal = Just (NewLayerDialog layerName)
                }

        _ ->
            Model inner


closeModal : Model -> Model
closeModal (Model inner) =
    Model
        { inner
            | modal = Nothing
        }



-- LAYERS


deleteSelectedLayer : Model -> Model
deleteSelectedLayer (Model inner) =
    case inner.selectedLayerIndexAndTool of
        Just ( index, _ ) ->
            Model
                { inner
                    | layers = remove index inner.layers
                    , selectedLayerIndexAndTool = Nothing
                }

        _ ->
            Model inner


createLayer : String -> Model -> Model
createLayer name (Model inner) =
    let
        newLayer =
            Layer.init name inner.width inner.height 0 0
    in
    Model
        { inner
            | layers = inner.layers ++ [ newLayer ]
        }


selectLayer : Maybe Int -> Model -> Model
selectLayer index (Model inner) =
    case index of
        Nothing ->
            Model
                { inner
                    | selectedLayerIndexAndTool = Nothing
                }

        Just i ->
            case getAt i inner.layers of
                Nothing ->
                    Model inner

                -- TODO:
                -- Don't update tool if selected layer doesn't change.
                -- Set the same type of tool that was previously in use if layer changes.
                Just _ ->
                    Model
                        { inner
                            | selectedLayerIndexAndTool = Just ( i, Pan )
                        }


setSelectedLayerMin : Int -> Model -> Model
setSelectedLayerMin newMin =
    updateSelectedLayer <| Layer.setMin newMin


setSelectedLayerMax : Int -> Model -> Model
setSelectedLayerMax newMax =
    updateSelectedLayer <| Layer.setMax newMax



-- TODO:
-- resize : Int -> Int -> Model
-- getTool : Model -> Tool
-- selectBrushTool : Model -> Model
-- selectPanTool : Model -> Model
-- stroke stuff
-- HELPER FUNCTIONS


updateSelectedLayer : (Layer -> Layer) -> Model -> Model
updateSelectedLayer f (Model inner) =
    case inner.selectedLayerIndexAndTool of
        Just ( selectedIndex, _ ) ->
            case getAt selectedIndex inner.layers of
                Just _ ->
                    let
                        updatedLayers =
                            List.indexedMap
                                (\i layer ->
                                    if i == selectedIndex then
                                        f layer

                                    else
                                        layer
                                )
                                inner.layers
                    in
                    Model { inner | layers = updatedLayers }

                _ ->
                    Model inner

        _ ->
            Model inner



-- getSelectedLayer : Model -> Maybe Layer
-- getSelectedLayer (Model inner) =
--     inner.selectedLayerIndexAndTool
--         |> Maybe.map (\( i, _ ) -> i)
--         |> Maybe.andThen (flip getAt inner.layers)


{-| Remove the element at the given index in the list.
-}
remove : Int -> List a -> List a
remove index xs =
    List.take index xs ++ List.drop (index + 1) xs



-- flip : (a -> b -> c) -> (b -> a -> c)
-- flip f =
--     \b a -> f a b
