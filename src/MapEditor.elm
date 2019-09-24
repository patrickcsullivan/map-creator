module MapEditor exposing (Msg, State, init, subscriptions, update, view)

import Browser.Events
import Brush exposing (Brush)
import Color
import DiscreteGradient exposing (DiscreteGradient)
import DiscreteGradientEditor
import File exposing (File)
import File.Download
import File.Select
import Grid exposing (Grid)
import GridEditor
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Layer exposing (Layer)
import LayerSelection exposing (LayerSelection)
import List.Extra as List
import Maybe.Extra as Maybe
import Task as Task



--------------------------------------------------------------------------------
-- MODEL


type alias State =
    { name : String
    , mapWidth : Int
    , mapHeight : Int
    , layerSelection : LayerSelection
    , brush : Brush
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
            25

        defaultMapHeight =
            25

        initLayer =
            makeLayerWithDefaultMinMax defaultLayerName defaultMapWidth defaultMapHeight

        initLayerSelection =
            LayerSelection.singleton <| initLayer

        initBrush =
            Brush.makeBrush Brush.Circle 3 (Layer.getMin initLayer)

        initGridEditor =
            makeGridEditor
                initLayer
                initBrush
                (gridEditorPaneWidth windowWidth)
                (gridEditorPaneHeight windowHeight)
    in
    { name = "map.json"
    , mapWidth = defaultMapWidth
    , mapHeight = defaultMapHeight
    , layerSelection = initLayerSelection
    , brush = initBrush
    , gridEditor = initGridEditor
    , dialog = Nothing
    , windowWidth = windowWidth
    , windowHeight = windowHeight
    }


makeGridEditor : Layer -> Brush -> Int -> Int -> GridEditor.State
makeGridEditor layer brush paneWidth paneHeight =
    GridEditor.init
        (Layer.getGrid layer)
        (Layer.getMin layer)
        (Layer.getMax layer)
        (Layer.getColorGradient layer)
        brush
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
    | WindowResize Int Int
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
    | SetBrushWidth Int
    | SetBrushPaintValue Int
    | OpenGradientEditorDialog
    | GradientEditorMsg DiscreteGradientEditor.Msg
    | GridEditorMsg GridEditor.Msg
    | Download
    | SavedFileRequested
    | SavedFileSelected File
    | SavedFileLoaded String
    | NoOp


withNoCmd : a -> ( a, Cmd Msg )
withNoCmd x =
    ( x, Cmd.none )


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ElapsedTime t ->
            { state
                | gridEditor = GridEditor.updateWithElapsedTime t state.gridEditor
            }
                |> withNoCmd

        WindowResize w h ->
            { state
                | gridEditor =
                    GridEditor.updateCanvasSize
                        (toFloat <| gridEditorPaneWidth w)
                        (toFloat <| gridEditorPaneHeight h)
                        state.gridEditor
            }
                |> withNoCmd

        SetMapWidth w ->
            if w > 0 then
                { state
                    | mapWidth = w
                    , layerSelection = LayerSelection.map (Layer.resizeGrid w state.mapHeight) state.layerSelection
                }
                    |> loadSelecteLayerIntoGridEditor
                    |> withNoCmd

            else
                state
                    |> withNoCmd

        SetMapHeight h ->
            if h > 0 then
                { state
                    | mapHeight = h
                    , layerSelection = LayerSelection.map (Layer.resizeGrid state.mapWidth h) state.layerSelection
                }
                    |> loadSelecteLayerIntoGridEditor
                    |> withNoCmd

            else
                state
                    |> withNoCmd

        SelectLayer index ->
            { state
                | layerSelection = LayerSelection.select index state.layerSelection
            }
                |> loadSelecteLayerIntoGridEditor
                |> withNoCmd

        DeleteSelectedLayer ->
            { state
                | layerSelection = LayerSelection.deleteSelected state.layerSelection
            }
                |> loadSelecteLayerIntoGridEditor
                |> withNoCmd

        SetLayerMin newMin ->
            { state
                | layerSelection = LayerSelection.mapSelected (Layer.setMin newMin) state.layerSelection
            }
                |> loadSelecteLayerIntoGridEditor
                |> withNoCmd

        SetLayerMax newMax ->
            { state
                | layerSelection = LayerSelection.mapSelected (Layer.setMax newMax) state.layerSelection
            }
                |> loadSelecteLayerIntoGridEditor
                |> withNoCmd

        SetBrushWidth width ->
            let
                isIncreasing =
                    width > Brush.width state.brush

                nextWidth =
                    if Basics.modBy width 2 == 1 then
                        width

                    else if isIncreasing then
                        width + 1

                    else
                        width - 1

                nextBrush =
                    Brush.makeBrush Brush.Circle nextWidth state.brush.paintValue
            in
            { state
                | brush = nextBrush
                , gridEditor = GridEditor.updateBrush nextBrush state.gridEditor
            }
                |> withNoCmd

        SetBrushPaintValue paintVal ->
            let
                -- TODO: Clean up.
                layer =
                    LayerSelection.selectedLayer state.layerSelection

                nextPaintVal =
                    boundInt (Layer.getMin layer) (Layer.getMax layer) paintVal

                nextBrush =
                    Brush.makeBrush Brush.Circle (Brush.width state.brush) nextPaintVal
            in
            { state
                | brush = nextBrush
                , gridEditor = GridEditor.updateBrush nextBrush state.gridEditor
            }
                |> withNoCmd

        OpenNewLayerDialog ->
            { state
                | dialog = Just (NewLayerDialog defaultLayerName)
            }
                |> withNoCmd

        SetNewLayerDialogNameField layerName ->
            case state.dialog of
                Just (NewLayerDialog _) ->
                    { state
                        | dialog = Just (NewLayerDialog layerName)
                    }
                        |> withNoCmd

                _ ->
                    state
                        |> withNoCmd

        NewLayerDialogCancel ->
            { state
                | dialog = Nothing -- Close dialog.
            }
                |> withNoCmd

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
                        |> withNoCmd

                _ ->
                    state
                        |> withNoCmd

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
                |> withNoCmd

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
                                |> withNoCmd

                        DiscreteGradientEditor.Cancel ->
                            { state
                                | dialog = Nothing -- Close dialog.
                            }
                                |> withNoCmd

                        DiscreteGradientEditor.Save gradient ->
                            { state
                                | layerSelection = LayerSelection.mapSelected (Layer.setColorGradient gradient) state.layerSelection
                                , dialog = Nothing -- Close dialog.
                            }
                                |> loadSelecteLayerIntoGridEditor
                                |> withNoCmd

                _ ->
                    state
                        |> withNoCmd

        GridEditorMsg editorMsg ->
            let
                ( newGridEditor, grid ) =
                    GridEditor.update editorMsg state.gridEditor
            in
            { state
                | layerSelection = LayerSelection.mapSelected (Layer.setGrid grid) state.layerSelection
                , gridEditor = newGridEditor
            }
                |> withNoCmd

        Download ->
            let
                downloadCmd =
                    state
                        |> toJson
                        |> Encode.encode 2
                        |> File.Download.string state.name "application/json"
            in
            ( state, downloadCmd )

        SavedFileRequested ->
            ( state
            , File.Select.file [ "application/json" ] SavedFileSelected
            )

        SavedFileSelected file ->
            ( state
            , Task.perform SavedFileLoaded (File.toString file)
            )

        SavedFileLoaded content ->
            let
                nextState =
                    content
                        |> Decode.decodeString savedMapDecoder
                        |> Result.toMaybe
                        |> Maybe.andThen (tryToLoadDecodedContent state)
                        |> Maybe.withDefault state
            in
            ( nextState
            , Cmd.none
            )

        NoOp ->
            state |> withNoCmd



-- TODO: Clean up.


tryToLoadDecodedContent : State -> { mapWidth : Int, mapHeight : Int, layers : List Layer } -> Maybe State
tryToLoadDecodedContent state decodedContent =
    decodedContent.layers
        |> LayerSelection.fromList
        |> Maybe.map
            (\layerSelection ->
                let
                    selectedLayer =
                        LayerSelection.selectedLayer layerSelection

                    initBrush =
                        Brush.makeBrush Brush.Circle 3 (Layer.getMin selectedLayer)
                in
                { state
                    | mapWidth = decodedContent.mapWidth
                    , mapHeight = decodedContent.mapHeight
                    , layerSelection = layerSelection
                    , brush = initBrush
                }
            )
        |> Maybe.map loadSelecteLayerIntoGridEditor
        |> Maybe.map
            (\s ->
                { s
                    | gridEditor = GridEditor.updateBrush s.brush s.gridEditor
                }
            )


subscriptions : State -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta ElapsedTime
        , Browser.Events.onResize WindowResize
        ]


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


toSetBrushWidth : String -> Msg
toSetBrushWidth =
    stringToIntMsgOrNoOp SetBrushWidth


toSetBrushPaintValue : String -> Msg
toSetBrushPaintValue =
    stringToIntMsgOrNoOp SetBrushPaintValue


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
                |> GridEditor.updateCellMin (Layer.getMin layer)
                |> GridEditor.updateCellMax (Layer.getMax layer)
                |> GridEditor.updateGrid (Layer.getGrid layer)
                |> GridEditor.updateGradient (Layer.getColorGradient layer)
    }



--------------------------------------------------------------------------------
-- ENCODER / DECODER


toJson : State -> Encode.Value
toJson state =
    Encode.object
        [ ( "mapWidth", Encode.int state.mapWidth )
        , ( "mapHeight", Encode.int state.mapHeight )
        , ( "layers", Encode.list Layer.toJson <| LayerSelection.toList state.layerSelection )
        ]


savedMapDecoder : Decode.Decoder { mapWidth : Int, mapHeight : Int, layers : List Layer }
savedMapDecoder =
    Decode.map3
        (\mapWidth mapHeight layers ->
            { mapWidth = mapWidth
            , mapHeight = mapHeight
            , layers = layers
            }
        )
        (Decode.field "mapWidth" Decode.int)
        (Decode.field "mapHeight" Decode.int)
        (Decode.field "layers" <| Decode.list Layer.decoder)



-- let
--     defaultMapWidth =
--         25
--     defaultMapHeight =
--         25
--     initLayer =
--         makeLayerWithDefaultMinMax defaultLayerName defaultMapWidth defaultMapHeight
--     initLayerSelection =
--         LayerSelection.singleton <| initLayer
--     initBrush =
--         Brush.makeBrush Brush.Circle 3 (Layer.getMin initLayer)
--     initGridEditor =
--         makeGridEditor
--             initLayer
--             initBrush
--             (gridEditorPaneWidth windowWidth)
--             (gridEditorPaneHeight windowHeight)
-- in
-- { name = "untitled.map"
-- , mapWidth = defaultMapWidth
-- , mapHeight = defaultMapHeight
-- , layerSelection = initLayerSelection
-- , brush = initBrush
-- , gridEditor = initGridEditor
-- , dialog = Nothing
-- , windowWidth = windowWidth
-- , windowHeight = windowHeight
-- }
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
            [ downloadButton
            , openButton
            , widthHeightField state.mapWidth state.mapHeight
            ]
        , toolbarSectionHeader "Layer"
        , toolbarSectionContents
            [ layerField layerIndex (LayerSelection.toList state.layerSelection)
            , minMaxField layer
            , colorGradientField layer
            ]
        , toolbarSectionHeader "Brush"
        , toolbarSectionContents
            [ brushWidthField (Brush.width state.brush)
            , brushValueField state.brush.paintValue
            ]
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
    Html.Events.on "click" (Decode.succeed message)



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



-- BRUSH WIDTH FIELD


brushWidthField : Int -> Html Msg
brushWidthField brushWidth =
    Html.div [ Html.Attributes.class "brush-width-field" ]
        [ brushWidthFieldLabel
        , brushWidthRow brushWidth
        ]


brushWidthFieldLabel : Html Msg
brushWidthFieldLabel =
    Html.div [ Html.Attributes.class "brush-width-field__label" ]
        [ Html.text "Width"
        ]


brushWidthRow : Int -> Html Msg
brushWidthRow brushWidth =
    Html.div [ Html.Attributes.class "brush-width-field__row" ]
        [ brushWidthInput brushWidth
        ]


brushWidthInput : Int -> Html Msg
brushWidthInput brushWidth =
    Html.input
        [ Html.Attributes.class "brush-width-field__input"
        , Html.Attributes.type_ "number"
        , Html.Attributes.value <| String.fromInt brushWidth
        , Html.Events.onInput toSetBrushWidth
        ]
        []



-- BRUSH VALUE FIELD


brushValueField : Int -> Html Msg
brushValueField brushValue =
    Html.div [ Html.Attributes.class "brush-value-field" ]
        [ brushValueFieldLabel
        , brushValueRow brushValue
        ]


brushValueFieldLabel : Html Msg
brushValueFieldLabel =
    Html.div [ Html.Attributes.class "brush-value-field__label" ]
        [ Html.text "Value"
        ]


brushValueRow : Int -> Html Msg
brushValueRow brushValue =
    Html.div [ Html.Attributes.class "brush-value-field__row" ]
        [ brushValueInput brushValue
        ]


brushValueInput : Int -> Html Msg
brushValueInput brushValue =
    Html.input
        [ Html.Attributes.class "brush-value-field__input"
        , Html.Attributes.type_ "number"
        , Html.Attributes.value <| String.fromInt brushValue
        , Html.Events.onInput toSetBrushPaintValue
        ]
        []



-- DOWNLOAD / OPEN BUTTONS


downloadButton : Html Msg
downloadButton =
    Html.button
        [ Html.Events.onClick Download
        , Html.Attributes.style "margin-bottom" "8px"
        , Html.Attributes.style "width" "100px"
        ]
        [ Html.text "Download"
        ]


openButton : Html Msg
openButton =
    Html.button
        [ Html.Events.onClick SavedFileRequested
        , Html.Attributes.style "margin-bottom" "8px"
        , Html.Attributes.style "width" "100px"
        ]
        [ Html.text "Open"
        ]



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



--------------------------------------------------------------------------------
-- HELPERS


boundInt : Int -> Int -> Int -> Int
boundInt lower upper =
    -- TODO: Factor out from here and in GridEditor.
    max lower << min upper
