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
import List.Extra as List
import Maybe.Extra as Maybe



--------------------------------------------------------------------------------
-- MODEL


type alias State =
    { name : String
    , mapWidth : Int
    , mapHeight : Int
    , layers : List Layer
    , layerSelection : Maybe LayerSelection
    , dialog : Maybe Dialog
    , windowWidth : Int
    , windowHeight : Int
    }



-- Invariants:
-- Width must be greater than 0.
-- Height must be greater than 0.
-- All layers must have same width and same height.
-- If selected layer index exists it must be greater than 0 and less than the number of layers.


type alias LayerSelection =
    { layerIndex : Int
    , tool : Tool
    , gridEditor : GridEditor.State
    }


type Tool
    = Pan
    | Brush Brush


type Dialog
    = NewLayerDialog String
    | GradientEditorDialog DiscreteGradientEditor.State


init : ( Int, Int ) -> ( State, Cmd Msg )
init ( windowWidth, windowHeight ) =
    ( { name = "untitled"
      , mapWidth = 10
      , mapHeight = 10
      , layers = []
      , layerSelection = Nothing
      , dialog = Nothing
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      }
    , Cmd.none
    )


initLayerSelection : List Layer -> Int -> Int -> Int -> Maybe LayerSelection
initLayerSelection layers selectedIndex windowWidth windowHeight =
    List.getAt selectedIndex layers
        |> Maybe.map
            (\layer ->
                { layerIndex = selectedIndex

                -- TODO:
                -- Don't update tool if selected layer doesn't change.
                -- Set the same type of tool that was previously in use if layer changes.
                , tool = Pan
                , gridEditor =
                    initGridEditor
                        layer
                        (gridEditorPaneWidth windowWidth)
                        (gridEditorPaneWidth windowHeight)
                }
            )


initGridEditor : Layer -> Int -> Int -> GridEditor.State
initGridEditor layer paneWidth paneHeight =
    GridEditor.init
        (Layer.getGrid layer)
        (Layer.getMin layer)
        (Layer.getMax layer)
        (Layer.getColorGradient layer)
        paneWidth
        paneHeight



--------------------------------------------------------------------------------
-- UPDATE


type Msg
    = WindowResize Int Int
    | SetMapWidth Int
    | SetMapHeight Int
    | SelectLayer Int
    | UnselectLayer
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


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    ( update_ msg state, Cmd.none )


update_ : Msg -> State -> State
update_ msg state =
    case msg of
        WindowResize width height ->
            { state
                | windowWidth = width
                , windowHeight = height
            }
                |> updateGridEditorPaneSize
                    (gridEditorPaneWidth width)
                    (gridEditorPaneHeight height)

        SetMapWidth w ->
            if w > 0 then
                { state
                    | mapWidth = w
                    , layers = List.map (Layer.resizeGrid w state.mapHeight) state.layers
                }
                    -- TODO: Refactor layers and layer selection so this isn't necessary.
                    |> (\newState ->
                            { newState
                                | layerSelection =
                                    getSelectedLayerIndex state
                                        |> Maybe.andThen (\i -> initLayerSelection newState.layers i newState.windowWidth newState.windowHeight)
                            }
                       )

            else
                state

        SetMapHeight h ->
            if h > 0 then
                { state
                    | mapHeight = h
                    , layers = List.map (Layer.resizeGrid state.mapWidth h) state.layers
                }
                    -- TODO: Refactor layers and layer selection so this isn't necessary.
                    |> (\newState ->
                            { newState
                                | layerSelection =
                                    getSelectedLayerIndex state
                                        |> Maybe.andThen (\i -> initLayerSelection newState.layers i newState.windowWidth newState.windowHeight)
                            }
                       )

            else
                state

        SelectLayer index ->
            selectLayer index state

        UnselectLayer ->
            { state
                | layerSelection = Nothing
            }

        DeleteSelectedLayer ->
            deleteSelectedLayer state

        OpenNewLayerDialog ->
            openNewLayerDialog state

        SetNewLayerDialogNameField layerName ->
            setNewLayerDialogName layerName state

        NewLayerDialogCancel ->
            closeDialog state

        NewLayerDialogCreate ->
            case getDialog state of
                Just (NewLayerDialog layerName) ->
                    state
                        |> createLayer layerName
                        |> selectLayer (getLayerCount state)
                        |> closeDialog

                _ ->
                    state

        SetLayerMin newMin ->
            updateSelectedLayerMin newMin state

        SetLayerMax newMax ->
            updateSelectedLayerMax newMax state

        OpenGradientEditorDialog ->
            openGradientEditorDialog state

        GradientEditorMsg editorMsg ->
            case getDialog state of
                Just (GradientEditorDialog dialog) ->
                    let
                        ( newDialog, output ) =
                            DiscreteGradientEditor.update editorMsg dialog
                    in
                    case output of
                        DiscreteGradientEditor.EditInProgress ->
                            updateGradientEditorDialog newDialog state

                        DiscreteGradientEditor.Cancel ->
                            closeDialog state

                        DiscreteGradientEditor.Save gradient ->
                            state
                                |> updateSelectedLayerColorGradient gradient
                                |> updateGridEditorGradient gradient
                                |> closeDialog

                _ ->
                    state

        GridEditorMsg editorMsg ->
            case getGridEditor state of
                Just gridEditor ->
                    let
                        ( newGridEditor, grid ) =
                            GridEditor.update editorMsg gridEditor
                    in
                    state |> updateSelectedLayerGrid grid

                _ ->
                    state

        NoOp ->
            state


subscriptions : State -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResize


toSetMapWidth : String -> Msg
toSetMapWidth =
    stringToIntMsgOrNoOp SetMapWidth


toSetMapHeight : String -> Msg
toSetMapHeight =
    stringToIntMsgOrNoOp SetMapHeight


toSelectLayerMsg : String -> Msg
toSelectLayerMsg s =
    s
        |> String.toInt
        |> Maybe.map SelectLayer
        |> Maybe.withDefault UnselectLayer


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



-- MODAL / DIALOGS


getDialog : State -> Maybe Dialog
getDialog state =
    state.dialog


closeDialog : State -> State
closeDialog state =
    { state
        | dialog = Nothing
    }


openNewLayerDialog : State -> State
openNewLayerDialog state =
    { state
        | dialog = Just (NewLayerDialog "New Layer")
    }


setNewLayerDialogName : String -> State -> State
setNewLayerDialogName layerName state =
    case state.dialog of
        Just (NewLayerDialog _) ->
            { state
                | dialog = Just (NewLayerDialog layerName)
            }

        _ ->
            state


openGradientEditorDialog : State -> State
openGradientEditorDialog state =
    case getSelectedLayer state of
        Just layer ->
            let
                editorState =
                    DiscreteGradientEditor.init
                        (Layer.getColorGradient layer)
                        (Layer.getMin layer)
                        (Layer.getMax layer)
            in
            { state
                | dialog = Just (GradientEditorDialog editorState)
            }

        _ ->
            state


updateGradientEditorDialog : DiscreteGradientEditor.State -> State -> State
updateGradientEditorDialog dialog state =
    case state.dialog of
        Just (GradientEditorDialog _) ->
            { state
                | dialog = Just (GradientEditorDialog dialog)
            }

        _ ->
            state



-- LAYERS


getLayerCount : State -> Int
getLayerCount state =
    state.layers |> List.length


getSelectedLayerIndex : State -> Maybe Int
getSelectedLayerIndex state =
    state.layerSelection
        |> Maybe.map (\s -> s.layerIndex)


getSelectedLayer : State -> Maybe Layer
getSelectedLayer state =
    state
        |> getSelectedLayerIndex
        |> Maybe.andThen (flip List.getAt state.layers)


deleteSelectedLayer : State -> State
deleteSelectedLayer state =
    case getSelectedLayerIndex state of
        Just index ->
            { state
                | layers = remove index state.layers
                , layerSelection = Nothing
            }

        _ ->
            state


createLayer : String -> State -> State
createLayer name state =
    let
        newLayer =
            Layer.init name state.mapWidth state.mapHeight 0 0
    in
    { state
        | layers = state.layers ++ [ newLayer ]
    }


selectLayer : Int -> State -> State
selectLayer index state =
    { state
        | layerSelection = initLayerSelection state.layers index state.windowWidth state.windowHeight
    }


updateSelectedLayerMin : Int -> State -> State
updateSelectedLayerMin =
    updateSelectedLayer << Layer.setMin


updateSelectedLayerMax : Int -> State -> State
updateSelectedLayerMax =
    updateSelectedLayer << Layer.setMax


updateSelectedLayerColorGradient : DiscreteGradient -> State -> State
updateSelectedLayerColorGradient =
    updateSelectedLayer << Layer.setColorGradient


updateSelectedLayerGrid : Grid Int -> State -> State
updateSelectedLayerGrid =
    updateSelectedLayer << Layer.setGrid



-- TODO:
-- resize : Int -> Int -> State
-- selectBrushTool : State -> State
-- selectPanTool : State -> State
-- stroke stuff


updateSelectedLayer : (Layer -> Layer) -> State -> State
updateSelectedLayer f state =
    case getSelectedLayerIndex state of
        Just selectedIndex ->
            case List.getAt selectedIndex state.layers of
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
                                state.layers
                    in
                    { state | layers = updatedLayers }

                _ ->
                    state

        _ ->
            state



-- TOOL


getSelectedTool : State -> Maybe Tool
getSelectedTool state =
    state.layerSelection
        |> Maybe.map (\s -> s.tool)



-- GRID EDITOR


getGridEditor : State -> Maybe GridEditor.State
getGridEditor state =
    state.layerSelection
        |> Maybe.map (\s -> s.gridEditor)


updateGridEditorPaneSize : Int -> Int -> State -> State
updateGridEditorPaneSize paneWidth paneHeight =
    updateGridEditor (GridEditor.updatePaneSize paneWidth paneHeight)


updateGridEditorGradient : DiscreteGradient -> State -> State
updateGridEditorGradient gradient =
    updateGridEditor (GridEditor.updateGradient gradient)


updateGridEditor : (GridEditor.State -> GridEditor.State) -> State -> State
updateGridEditor f state =
    { state
        | layerSelection = state.layerSelection |> Maybe.map (updateGridEditor_ f)
    }


updateGridEditor_ : (GridEditor.State -> GridEditor.State) -> LayerSelection -> LayerSelection
updateGridEditor_ f selection =
    { selection
        | gridEditor = f selection.gridEditor
    }



--------------------------------------------------------------------------------
-- VIEW


view : State -> Html Msg
view state =
    let
        content =
            [ toolbar state
            , gridEditorPaneView (getGridEditor state)
            ]
                ++ (dialogView <| getDialog state)
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


gridEditorPaneView : Maybe GridEditor.State -> Html Msg
gridEditorPaneView maybeGridEditor =
    let
        content =
            maybeGridEditor
                |> Maybe.map GridEditor.view
                |> Maybe.map (Html.map GridEditorMsg)
                |> Maybe.toList

        -- layer
        -- |> Maybe.map GridEditor.view
    in
    Html.div [ Html.Attributes.class "grid-editor-pane" ]
        content



-- TOOLBAR


toolbar : State -> Html Msg
toolbar state =
    let
        layerIndex =
            getSelectedLayerIndex state

        layer =
            getSelectedLayer state

        tool =
            getSelectedTool state
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
            [ layerField layerIndex state.layers
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


layerField : Maybe Int -> List Layer -> Html Msg
layerField selectedIndex layers =
    Html.div
        [ Html.Attributes.class "layer-field"
        ]
        [ layerFieldSelect selectedIndex layers
        , layerFieldNewButton
        , layerFieldDeleteButton
        ]


layerFieldSelect : Maybe Int -> List Layer -> Html Msg
layerFieldSelect selectedIndex layers =
    let
        emptyOption =
            layerFieldEmptyOption (Maybe.isNothing selectedIndex)

        options =
            [ emptyOption ] ++ layerFieldOptions selectedIndex layers
    in
    Html.select
        [ Html.Attributes.class "layer-field__select"
        , Html.Events.onInput toSelectLayerMsg
        ]
        options


layerFieldEmptyOption : Bool -> Html Msg
layerFieldEmptyOption isSelected =
    Html.option
        [ Html.Attributes.value ""
        , Html.Attributes.selected isSelected
        ]
        [ Html.text ""
        ]


layerFieldOptions : Maybe Int -> List Layer -> List (Html Msg)
layerFieldOptions selectedIndex =
    List.indexedMap
        (\i layer ->
            layerFieldOption i layer (isJustEqual selectedIndex i)
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


minMaxField : Maybe Layer -> Html Msg
minMaxField layer =
    Html.div [ Html.Attributes.class "min-max-field" ]
        [ minMaxFieldLabel
        , minInput <| Maybe.map Layer.getMin layer
        , minMaxFieldInputSeparator
        , maxInput <| Maybe.map Layer.getMax layer
        ]


minMaxFieldLabel : Html Msg
minMaxFieldLabel =
    Html.div [ Html.Attributes.class "min-max-field__label" ]
        [ Html.text "Min / Max"
        ]


minInput : Maybe Int -> Html Msg
minInput val =
    case val of
        Just v ->
            enabledMinMaxFieldInput v toSetLayerMinMsg

        Nothing ->
            disabledMinMaxFieldInput


maxInput : Maybe Int -> Html Msg
maxInput val =
    case val of
        Just v ->
            enabledMinMaxFieldInput v toSetLayerMaxMsg

        Nothing ->
            disabledMinMaxFieldInput


enabledMinMaxFieldInput : Int -> (String -> Msg) -> Html Msg
enabledMinMaxFieldInput val toMsg =
    Html.input
        [ Html.Attributes.class "min-max-field__input"
        , Html.Attributes.type_ "number"
        , Html.Attributes.value <| String.fromInt val
        , Html.Events.onInput toMsg
        ]
        []


disabledMinMaxFieldInput : Html Msg
disabledMinMaxFieldInput =
    Html.input
        [ Html.Attributes.class "min-max-field__input"
        , Html.Attributes.type_ "number"
        , Html.Attributes.disabled True
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


colorGradientField : Maybe Layer -> Html Msg
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


colorGradientRow : Maybe Layer -> Html Msg
colorGradientRow maybeLayer =
    let
        contents =
            case maybeLayer of
                Just layer ->
                    [ enabledColorGradient
                        (Layer.getColorGradient layer)
                        (Layer.getMin layer)
                        (Layer.getMax layer)
                    , colorGradientEditButton False
                    ]

                Nothing ->
                    [ disabledColorGradient
                    , colorGradientEditButton True
                    ]
    in
    Html.div [ Html.Attributes.class "color-gradient__row" ]
        contents


enabledColorGradient : DiscreteGradient -> Int -> Int -> Html Msg
enabledColorGradient gradient layerMin layerMax =
    Html.div
        [ Html.Attributes.class "color-gradient"
        ]
        [ simpleGradient gradient layerMin layerMax
        ]


disabledColorGradient : Html Msg
disabledColorGradient =
    Html.div
        [ Html.Attributes.class "color-gradient"
        , Html.Attributes.class "color-gradient_disabled"
        ]
        []


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



--------------------------------------------------------------------------------
-- HELPERS


isJustEqual : Maybe a -> a -> Bool
isJustEqual maybeX y =
    case maybeX of
        Nothing ->
            False

        Just x ->
            x == y


{-| Remove the element at the given index in the list.
-}
remove : Int -> List a -> List a
remove index xs =
    List.take index xs ++ List.drop (index + 1) xs


{-| Flip the function's parameter order.
-}
flip : (a -> b -> c) -> (b -> a -> c)
flip f =
    \b a -> f a b
