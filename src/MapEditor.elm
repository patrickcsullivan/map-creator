module MapEditor exposing (Msg, State, init, update, view)

import Brush exposing (Brush)
import Color
import DiscreteGradient exposing (DiscreteGradient)
import DiscreteGradientEditor
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
    , width : Int
    , height : Int
    , layers : List Layer
    , layerSelection : Maybe LayerSelection
    , dialog : Maybe Dialog
    , windowWidth : Float
    , windowHeight : Float
    }



-- Invariants:
-- Width must be greater than 0.
-- Height must be greater than 0.
-- All layers must have same width and same height.
-- If selected layer index exists it must be greater than 0 and less than the number of layers.


type Tool
    = Pan
    | Brush Brush


type alias LayerSelection =
    { layerIndex : Int
    , tool : Tool
    , gridEditor : GridEditor.State
    }


type Dialog
    = NewLayerDialog String
    | GradientEditorDialog DiscreteGradientEditor.State


init : ( Float, Float ) -> ( State, Cmd Msg )
init ( windowWidth, windowHeight ) =
    ( { name = "untitled"
      , width = 10
      , height = 10
      , layers = []
      , layerSelection = Nothing
      , dialog = Nothing
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      }
    , Cmd.none
    )


initGridEditor : Layer -> Float -> Float -> GridEditor.State
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
    = SelectLayer (Maybe Int)
    | DeleteSelectedLayer
    | OpenNewLayerDialog
    | SetNewLayerDialogNameField String
    | NewLayerDialogCancel
    | NewLayerDialogCreate
    | SetLayerMin (Maybe Int)
    | SetLayerMax (Maybe Int)
    | OpenGradientEditorDialog
    | GradientEditorMsg DiscreteGradientEditor.Msg


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    (case msg |> Debug.log "msg" of
        SelectLayer index ->
            selectLayer index state

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
                        |> selectLayer (Just (getLayerCount state))
                        |> closeDialog

                _ ->
                    state

        SetLayerMin newMin ->
            case newMin of
                Just m ->
                    updateSelectedLayerMin m state

                Nothing ->
                    state

        SetLayerMax newMax ->
            case newMax of
                Just m ->
                    updateSelectedLayerMax m state

                Nothing ->
                    state

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
                                |> closeDialog

                _ ->
                    state
    )
        |> Debug.log "State"
        |> (\s -> ( s, Cmd.none ))


toSelectLayerMsg : String -> Msg
toSelectLayerMsg =
    SelectLayer << String.toInt


toSetLayerMinMsg : String -> Msg
toSetLayerMinMsg =
    SetLayerMin << String.toInt


toSetLayerMaxMsg : String -> Msg
toSetLayerMaxMsg =
    SetLayerMax << String.toInt


getSelectedLayerIndex : State -> Maybe Int
getSelectedLayerIndex state =
    state.layerSelection
        |> Maybe.map (\s -> s.layerIndex)


getSelectedLayer : State -> Maybe Layer
getSelectedLayer state =
    state
        |> getSelectedLayerIndex
        |> Maybe.andThen (flip List.getAt state.layers)


getSelectedTool : State -> Maybe Tool
getSelectedTool state =
    state.layerSelection
        |> Maybe.map (\s -> s.tool)



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
            Layer.init name state.width state.height 0 0
    in
    { state
        | layers = state.layers ++ [ newLayer ]
    }


selectLayer : Maybe Int -> State -> State
selectLayer index state =
    case index of
        Nothing ->
            { state
                | layerSelection = Nothing
            }

        Just i ->
            case List.getAt i state.layers of
                Nothing ->
                    state

                Just layer ->
                    { state
                        | layerSelection =
                            Just
                                { layerIndex = i

                                -- TODO:
                                -- Don't update tool if selected layer doesn't change.
                                -- Set the same type of tool that was previously in use if layer changes.
                                , tool = Pan
                                , gridEditor =
                                    initGridEditor
                                        layer
                                        (gridEditorPaneWidth state.windowWidth)
                                        state.windowHeight
                                }
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



-- TODO:
-- resize : Int -> Int -> State
-- getTool : State -> Tool
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


getLayerCount : State -> Int
getLayerCount state =
    state.layers |> List.length



-- getSelectedLayer : State -> Maybe Layer
-- getSelectedLayer state =
--     state.selectedLayerIndexAndTool
--         |> Maybe.map (\( i, _ ) -> i)
--         |> Maybe.andThen (flip getAt state.layers)
--------------------------------------------------------------------------------
-- VIEW


view : State -> Html Msg
view state =
    let
        content =
            [ toolbar state
            , gridEditorPaneView (getSelectedLayer state)
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


gridEditorPaneView : Maybe Layer -> Html Msg
gridEditorPaneView layer =
    let
        content =
            []

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
    Html.div [ Html.Attributes.class "toolbar" ]
        [ toolbarSectionHeader "Layer"
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



-- MIN / MAX CONTROL


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



--------------------------------------------------------------------------------
-- LAYOUT CALCULATIONS


toolbarWidth : Float
toolbarWidth =
    300.0


gridEditorPaneWidth : Float -> Float
gridEditorPaneWidth windowWidth =
    max 0.0 (windowWidth - toolbarWidth)



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
