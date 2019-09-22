module MapEditor exposing (Msg, State, init, subscriptions, update, view)

import Browser.Events
import Brush exposing (Brush)
import Color
import DiscreteGradient exposing (DiscreteGradient)
import DiscreteGradientEditor
import Grid exposing (Grid)
import GridEditor
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Layer exposing (Layer)
import LayerSelection exposing (LayerSelection)
import List.Extra as List
import Maybe.Extra as Maybe



--------------------------------------------------------------------------------
-- MODEL


type alias State =
    { name : String
    , mapWidth : Int
    , mapHeight : Int
    , layerSelection : LayerSelection
    , gridEditor : GridEditor.State
    , dialog : Maybe Dialog
    , windowWidth : Int
    , windowHeight : Int
    }



-- Invariants:
-- Width must be greater than 0.
-- Height must be greater than 0.
-- All layers must have same width and same height.
-- If selected layer index exists it must be greater than 0 and less than the number of layers.


type Dialog
    = NewLayerDialog String
    | GradientEditorDialog DiscreteGradientEditor.State


init : ( Int, Int ) -> State
init ( windowWidth, windowHeight ) =
    let
        defaultMapWidth =
            20

        defaultMapHeight =
            20

        initLayer =
            makeLayerWithDefaultMinMax defaultLayerName defaultMapWidth defaultMapHeight

        initLayerSelection =
            LayerSelection.singleton <| initLayer

        initGridEditor =
            makeGridEditor
                initLayer
                (gridEditorPaneWidth windowWidth)
                (gridEditorPaneHeight windowHeight)
    in
    { name = "untitled"
    , mapWidth = defaultMapWidth
    , mapHeight = defaultMapHeight
    , layerSelection = initLayerSelection
    , gridEditor = initGridEditor
    , dialog = Nothing
    , windowWidth = windowWidth
    , windowHeight = windowHeight
    }


makeGridEditor : Layer -> Int -> Int -> GridEditor.State
makeGridEditor layer paneWidth paneHeight =
    GridEditor.init
        (Layer.getGrid layer)
        -- (Layer.getMin layer)
        -- (Layer.getMax layer)
        (Layer.getColorGradient layer)
        (toFloat paneWidth)
        (toFloat paneHeight)


{-| Default name for new layers.
-}
defaultLayerName : String
defaultLayerName =
    "New Layer"


{-| Make a layer with default cell min and max.
-}
makeLayerWithDefaultMinMax : String -> Int -> Int -> Layer
makeLayerWithDefaultMinMax name width height =
    let
        defaultCellMin =
            0

        defaultCellMax =
            9
    in
    Layer.init name width height defaultCellMin defaultCellMax



--------------------------------------------------------------------------------
-- UPDATE


type Msg
    = ElapsedTime Float
    | SetMapWidth Int
    | SetMapHeight Int
    | SelectLayer Int
    | DeleteSelectedLayer
    | OpenNewLayerDialog
    | SetNewLayerDialogNameField String
    | NewLayerDialogCancel
    | NewLayerDialogCreate
    | SetLayerMin Int
    | SetLayerMax Int
    | OpenGradientEditorDialog
    | GradientEditorMsg DiscreteGradientEditor.Msg
    | GridEditorMsg GridEditor.Msg
    | NoOp


update : Msg -> State -> State
update msg state =
    case msg of
        ElapsedTime t ->
            { state
                | gridEditor = GridEditor.updateWithElapsedTime t state.gridEditor
            }

        SetMapWidth w ->
            if w > 0 then
                { state
                    | mapWidth = w
                    , layerSelection = LayerSelection.map (Layer.resizeGrid w state.mapHeight) state.layerSelection
                }
                    |> loadSelecteLayerIntoGridEditor

            else
                state

        SetMapHeight h ->
            if h > 0 then
                { state
                    | mapHeight = h
                    , layerSelection = LayerSelection.map (Layer.resizeGrid state.mapWidth h) state.layerSelection
                }
                    |> loadSelecteLayerIntoGridEditor

            else
                state

        SelectLayer index ->
            { state
                | layerSelection = LayerSelection.select index state.layerSelection
            }
                |> loadSelecteLayerIntoGridEditor

        DeleteSelectedLayer ->
            { state
                | layerSelection = LayerSelection.deleteSelected state.layerSelection
            }
                |> loadSelecteLayerIntoGridEditor

        SetLayerMin newMin ->
            { state
                | layerSelection = LayerSelection.mapSelected (Layer.setMin newMin) state.layerSelection
            }
                |> loadSelecteLayerIntoGridEditor

        SetLayerMax newMax ->
            { state
                | layerSelection = LayerSelection.mapSelected (Layer.setMax newMax) state.layerSelection
            }
                |> loadSelecteLayerIntoGridEditor

        OpenNewLayerDialog ->
            { state
                | dialog = Just (NewLayerDialog defaultLayerName)
            }

        SetNewLayerDialogNameField layerName ->
            case state.dialog of
                Just (NewLayerDialog _) ->
                    { state
                        | dialog = Just (NewLayerDialog layerName)
                    }

                _ ->
                    state

        NewLayerDialogCancel ->
            { state
                | dialog = Nothing -- Close dialog.
            }

        NewLayerDialogCreate ->
            case state.dialog of
                Just (NewLayerDialog layerName) ->
                    { state
                        | layerSelection =
                            state.layerSelection
                                |> LayerSelection.add (makeLayerWithDefaultMinMax layerName state.mapWidth state.mapHeight)
                                |> LayerSelection.select (LayerSelection.length state.layerSelection)
                        , dialog = Nothing -- Close dialog.
                    }
                        |> loadSelecteLayerIntoGridEditor

                _ ->
                    state

        OpenGradientEditorDialog ->
            let
                layer =
                    LayerSelection.selectedLayer state.layerSelection

                editorState =
                    DiscreteGradientEditor.init
                        (Layer.getColorGradient layer)
                        (Layer.getMin layer)
                        (Layer.getMax layer)
            in
            { state
                | dialog = Just (GradientEditorDialog editorState)
            }

        GradientEditorMsg editorMsg ->
            case state.dialog of
                Just (GradientEditorDialog dialog) ->
                    let
                        ( newDialog, output ) =
                            DiscreteGradientEditor.update editorMsg dialog
                    in
                    case output of
                        DiscreteGradientEditor.EditInProgress ->
                            { state
                                | dialog = Just (GradientEditorDialog newDialog)
                            }

                        DiscreteGradientEditor.Cancel ->
                            { state
                                | dialog = Nothing -- Close dialog.
                            }

                        DiscreteGradientEditor.Save gradient ->
                            { state
                                | layerSelection = LayerSelection.mapSelected (Layer.setColorGradient gradient) state.layerSelection
                                , dialog = Nothing -- Close dialog.
                            }
                                |> loadSelecteLayerIntoGridEditor

                _ ->
                    state

        GridEditorMsg editorMsg ->
            let
                ( newGridEditor, grid ) =
                    GridEditor.update editorMsg state.gridEditor
            in
            { state
                | layerSelection = LayerSelection.mapSelected (Layer.setGrid grid) state.layerSelection
                , gridEditor = newGridEditor
            }

        NoOp ->
            state


subscriptions : State -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta ElapsedTime


toSetMapWidth : String -> Msg
toSetMapWidth =
    stringToIntMsgOrNoOp SetMapWidth


toSetMapHeight : String -> Msg
toSetMapHeight =
    stringToIntMsgOrNoOp SetMapHeight


toSelectLayerMsg : String -> Msg
toSelectLayerMsg =
    stringToIntMsgOrNoOp SelectLayer


toSetLayerMinMsg : String -> Msg
toSetLayerMinMsg =
    stringToIntMsgOrNoOp SetLayerMin


toSetLayerMaxMsg : String -> Msg
toSetLayerMaxMsg =
    stringToIntMsgOrNoOp SetLayerMax


stringToIntMsgOrNoOp : (Int -> Msg) -> String -> Msg
stringToIntMsgOrNoOp toMsg s =
    s
        |> String.toInt
        |> Maybe.map toMsg
        |> Maybe.withDefault NoOp


loadSelecteLayerIntoGridEditor : State -> State
loadSelecteLayerIntoGridEditor state =
    let
        layer =
            LayerSelection.selectedLayer state.layerSelection
    in
    { state
        | gridEditor =
            state.gridEditor
                -- |> GridEditor.updateCellMin (Layer.getMin layer)
                -- |> GridEditor.updateCellMax (Layer.getMax layer)
                |> GridEditor.updateGrid (Layer.getGrid layer)
                |> GridEditor.updateGradient (Layer.getColorGradient layer)
    }



--------------------------------------------------------------------------------
-- VIEW


view : State -> Html Msg
view state =
    let
        content =
            [ toolbar state
            , gridEditorPaneView state.gridEditor
            ]
                ++ dialogView state.dialog
    in
    Html.div [ Html.Attributes.class "page" ]
        content



-- DIALOG


dialogView : Maybe Dialog -> List (Html Msg)
dialogView dialog =
    case dialog of
        Just m ->
            [ Html.div [ Html.Attributes.class "mask" ]
                [ Html.div [ Html.Attributes.class "dialog" ]
                    [ dialogContent m
                    ]
                ]
            ]

        _ ->
            []


dialogContent : Dialog -> Html Msg
dialogContent dialog =
    case dialog of
        NewLayerDialog layerName ->
            newLayerDialog layerName

        GradientEditorDialog state ->
            gradientEditorDialog state


newLayerDialog : String -> Html Msg
newLayerDialog layerName =
    Html.div [ Html.Attributes.class "new-layer-dialog" ]
        [ Html.div [ Html.Attributes.class "dialog__header", Html.Attributes.class "new-layer-dialog__header" ] [ Html.text "New Layer" ]
        , Html.div [ Html.Attributes.class "new-layer-dialog__name-field" ]
            [ Html.div [ Html.Attributes.class "new-layer-dialog__name-label" ] [ Html.text "Name" ]
            , Html.input
                [ Html.Attributes.class "new-layer-dialog__name-label"
                , Html.Attributes.value layerName
                , Html.Events.onInput SetNewLayerDialogNameField
                ]
                []
            ]
        , Html.div [ Html.Attributes.class "new-layer-dialog__button-container" ]
            [ Html.button
                [ Html.Attributes.class "new-layer-dialog__button"
                , Html.Events.onClick NewLayerDialogCancel
                ]
                [ Html.text "Cancel" ]
            , Html.button
                [ Html.Attributes.class "new-layer-dialog__button"
                , Html.Events.onClick NewLayerDialogCreate
                ]
                [ Html.text "Create" ]
            ]
        ]


gradientEditorDialog : DiscreteGradientEditor.State -> Html Msg
gradientEditorDialog state =
    DiscreteGradientEditor.view state
        |> Html.map GradientEditorMsg



-- GRID EDITOR PANE


gridEditorPaneView : GridEditor.State -> Html Msg
gridEditorPaneView gridEditor =
    Html.div [ Html.Attributes.class "grid-editor-pane" ]
        [ GridEditor.view gridEditor
            |> Html.map GridEditorMsg
        ]



-- TOOLBAR


toolbar : State -> Html Msg
toolbar state =
    let
        layerIndex =
            LayerSelection.selectedIndex state.layerSelection

        layer =
            LayerSelection.selectedLayer state.layerSelection
    in
    Html.div
        [ Html.Attributes.class "toolbar"
        , Html.Attributes.style "width" (String.fromInt toolbarWidth ++ "px")
        ]
        [ toolbarSectionHeader "Map"
        , toolbarSectionContents
            [ widthHeightField state.mapWidth state.mapHeight
            ]
        , toolbarSectionHeader "Layer"
        , toolbarSectionContents
            [ layerField layerIndex (LayerSelection.toList state.layerSelection)
            , minMaxField layer
            , colorGradientField layer
            ]
        , toolbarSectionHeader "Tools"
        , toolbarSectionHeader "Brush"
        ]


toolbarSectionContents : List (Html Msg) -> Html Msg
toolbarSectionContents contents =
    Html.div [ Html.Attributes.class "toolbar__section-contents" ]
        contents


toolbarSectionHeader : String -> Html Msg
toolbarSectionHeader header =
    Html.div [ Html.Attributes.class "toolbar__section-header" ]
        [ Html.text header
        ]



-- WIDTH / HEIGHT FIELD


widthHeightField : Int -> Int -> Html Msg
widthHeightField mapWidth mapHeight =
    Html.div [ Html.Attributes.class "width-height-field" ]
        [ widthHeightFieldLabel
        , widthHeightFieldInput mapWidth toSetMapWidth
        , widthHeightFieldInputSeparator
        , widthHeightFieldInput mapHeight toSetMapHeight
        ]


widthHeightFieldLabel : Html Msg
widthHeightFieldLabel =
    Html.div [ Html.Attributes.class "width-height-field__label" ]
        [ Html.text "Width / Height"
        ]


widthHeightFieldInput : Int -> (String -> Msg) -> Html Msg
widthHeightFieldInput val toMsg =
    Html.input
        [ Html.Attributes.class "width-height-field__input"
        , Html.Attributes.type_ "number"
        , Html.Attributes.value <| String.fromInt val
        , Html.Events.onInput toMsg
        ]
        []


widthHeightFieldInputSeparator : Html Msg
widthHeightFieldInputSeparator =
    Html.div
        [ Html.Attributes.class "width-height-field__input-separator"
        ]
        [ Html.text "/"
        ]



-- LAYER FIELD


layerField : Int -> List Layer -> Html Msg
layerField selectedIndex layers =
    Html.div
        [ Html.Attributes.class "layer-field"
        ]
        [ layerFieldSelect selectedIndex layers
        , layerFieldNewButton
        , layerFieldDeleteButton
        ]


layerFieldSelect : Int -> List Layer -> Html Msg
layerFieldSelect selectedIndex layers =
    Html.select
        [ Html.Attributes.class "layer-field__select"
        , Html.Events.onInput toSelectLayerMsg
        ]
        (layerFieldOptions selectedIndex layers)


layerFieldOptions : Int -> List Layer -> List (Html Msg)
layerFieldOptions selectedIndex =
    List.indexedMap
        (\i layer ->
            layerFieldOption i layer (i == selectedIndex)
        )


layerFieldOption : Int -> Layer -> Bool -> Html Msg
layerFieldOption index layer isSelected =
    Html.option
        [ Html.Attributes.value <| String.fromInt index
        , Html.Attributes.selected isSelected
        ]
        [ Html.text <| Layer.getName layer
        ]


layerFieldNewButton : Html Msg
layerFieldNewButton =
    Html.button [ Html.Events.onClick OpenNewLayerDialog ] [ Html.text "+" ]


layerFieldDeleteButton : Html Msg
layerFieldDeleteButton =
    Html.button [ Html.Events.onClick DeleteSelectedLayer ] [ Html.text "-" ]



-- MIN / MAX FIELD


minMaxField : Layer -> Html Msg
minMaxField layer =
    Html.div [ Html.Attributes.class "min-max-field" ]
        [ minMaxFieldLabel
        , minInput <| Layer.getMin layer
        , minMaxFieldInputSeparator
        , maxInput <| Layer.getMax layer
        ]


minMaxFieldLabel : Html Msg
minMaxFieldLabel =
    Html.div [ Html.Attributes.class "min-max-field__label" ]
        [ Html.text "Min / Max"
        ]


minInput : Int -> Html Msg
minInput val =
    minMaxFieldInput val toSetLayerMinMsg


maxInput : Int -> Html Msg
maxInput val =
    minMaxFieldInput val toSetLayerMaxMsg


minMaxFieldInput : Int -> (String -> Msg) -> Html Msg
minMaxFieldInput val toMsg =
    Html.input
        [ Html.Attributes.class "min-max-field__input"
        , Html.Attributes.type_ "number"
        , Html.Attributes.value <| String.fromInt val
        , Html.Events.onInput toMsg
        ]
        []


minMaxFieldInputSeparator : Html Msg
minMaxFieldInputSeparator =
    Html.div
        [ Html.Attributes.class "min-max-field__input-separator"
        ]
        [ Html.text "/"
        ]


onClick : msg -> Html.Attribute msg
onClick message =
    Html.Events.on "click" (Json.Decode.succeed message)



-- COLOR FIELD


colorGradientField : Layer -> Html Msg
colorGradientField layer =
    Html.div [ Html.Attributes.class "color-field" ]
        [ colorFieldLabel
        , colorGradientRow layer
        ]


colorFieldLabel : Html Msg
colorFieldLabel =
    Html.div [ Html.Attributes.class "color-field__label" ]
        [ Html.text "Color"
        ]


colorGradientRow : Layer -> Html Msg
colorGradientRow layer =
    Html.div [ Html.Attributes.class "color-gradient__row" ]
        [ colorGradient
            (Layer.getColorGradient layer)
            (Layer.getMin layer)
            (Layer.getMax layer)
        , colorGradientEditButton False
        ]


colorGradient : DiscreteGradient -> Int -> Int -> Html Msg
colorGradient gradient layerMin layerMax =
    Html.div
        [ Html.Attributes.class "color-gradient"
        ]
        [ simpleGradient gradient layerMin layerMax
        ]


colorGradientEditButton : Bool -> Html Msg
colorGradientEditButton isDisabled =
    Html.button
        [ Html.Attributes.disabled isDisabled
        , Html.Events.onClick OpenGradientEditorDialog
        ]
        [ Html.text "Edit"
        ]


simpleGradient : DiscreteGradient -> Int -> Int -> Html Msg
simpleGradient gradient layerMin layerMax =
    let
        cells =
            List.map (simpleGradientCell gradient) (List.range layerMin layerMax)
    in
    Html.div [ Html.Attributes.class "simple-gradient" ]
        [ Html.div [ Html.Attributes.class "simple-gradient__row" ]
            cells
        ]


simpleGradientCell : DiscreteGradient -> Int -> Html Msg
simpleGradientCell gradient val =
    let
        color =
            DiscreteGradient.getColorAt val gradient
    in
    Html.div
        [ Html.Attributes.class "simple-gradient__cell"
        , Html.Attributes.style "background-color" (Color.toCssString color)
        ]
        []



--------------------------------------------------------------------------------
-- LAYOUT CALCULATIONS


toolbarWidth : Int
toolbarWidth =
    250


gridEditorPaneWidth : Int -> Int
gridEditorPaneWidth windowWidth =
    max 0 (windowWidth - toolbarWidth)


gridEditorPaneHeight : Int -> Int
gridEditorPaneHeight windowHeight =
    windowHeight
