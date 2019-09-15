module MapEditor exposing (init, update, view)

import Brush exposing (Brush)
import Color
import DiscreteGradient exposing (DiscreteGradient)
import DiscreteGradientEditor
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
    , selectedLayerIndexAndTool : Maybe ( Int, Tool )
    , modal : Maybe Modal
    }



-- Invariants:
-- Width must be greater than 0.
-- Height must be greater than 0.
-- All layers must have same width and same height.
-- If selected layer index exists it must be greater than 0 and less than the number of layers.


type Tool
    = Pan
    | Brush Brush


type Modal
    = NewLayerDialog String
    | GradientEditorDialog DiscreteGradientEditor.State


init : State
init =
    { name = "untitled"
    , width = 10
    , height = 10
    , layers = []
    , selectedLayerIndexAndTool = Nothing
    , modal = Nothing
    }



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


update : Msg -> State -> State
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
            closeModal state

        NewLayerDialogCreate ->
            case getModal state of
                Just (NewLayerDialog layerName) ->
                    state
                        |> createLayer layerName
                        |> selectLayer (Just (getLayerCount state))
                        |> closeModal

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
            case getModal state of
                Just (GradientEditorDialog dialog) ->
                    let
                        ( newDialog, output ) =
                            DiscreteGradientEditor.update editorMsg dialog
                    in
                    case output of
                        DiscreteGradientEditor.EditInProgress ->
                            updateGradientEditorDialog newDialog state

                        DiscreteGradientEditor.Cancel ->
                            closeModal state

                        DiscreteGradientEditor.Save gradient ->
                            state
                                |> updateSelectedLayerColorGradient gradient
                                |> closeModal

                _ ->
                    state
    )
        |> Debug.log "State"


toSelectLayerMsg : String -> Msg
toSelectLayerMsg =
    SelectLayer << String.toInt


toSetLayerMinMsg : String -> Msg
toSetLayerMinMsg =
    SetLayerMin << String.toInt


toSetLayerMaxMsg : String -> Msg
toSetLayerMaxMsg =
    SetLayerMax << String.toInt


getSelectedLayerAndTool : State -> Maybe ( Int, Layer, Tool )
getSelectedLayerAndTool state =
    case state.selectedLayerIndexAndTool of
        Nothing ->
            Nothing

        Just ( layerIndex, tool ) ->
            case List.getAt layerIndex state.layers of
                Nothing ->
                    Nothing

                Just layer ->
                    Just ( layerIndex, layer, tool )



-- MODAL / DIALOGS


getModal : State -> Maybe Modal
getModal state =
    state.modal


closeModal : State -> State
closeModal state =
    { state
        | modal = Nothing
    }


openNewLayerDialog : State -> State
openNewLayerDialog state =
    { state
        | modal = Just (NewLayerDialog "New Layer")
    }


setNewLayerDialogName : String -> State -> State
setNewLayerDialogName layerName state =
    case state.modal of
        Just (NewLayerDialog _) ->
            { state
                | modal = Just (NewLayerDialog layerName)
            }

        _ ->
            state


openGradientEditorDialog : State -> State
openGradientEditorDialog state =
    case getSelectedLayerAndTool state of
        Just ( _, layer, _ ) ->
            let
                editorState =
                    DiscreteGradientEditor.init
                        (Layer.getColorGradient layer)
                        (Layer.getMin layer)
                        (Layer.getMax layer)
            in
            { state
                | modal = Just (GradientEditorDialog editorState)
            }

        _ ->
            state


updateGradientEditorDialog : DiscreteGradientEditor.State -> State -> State
updateGradientEditorDialog dialog state =
    case state.modal of
        Just (GradientEditorDialog _) ->
            { state
                | modal = Just (GradientEditorDialog dialog)
            }

        _ ->
            state



-- LAYERS


deleteSelectedLayer : State -> State
deleteSelectedLayer state =
    case state.selectedLayerIndexAndTool of
        Just ( index, _ ) ->
            { state
                | layers = remove index state.layers
                , selectedLayerIndexAndTool = Nothing
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
                | selectedLayerIndexAndTool = Nothing
            }

        Just i ->
            case List.getAt i state.layers of
                Nothing ->
                    state

                -- TODO:
                -- Don't update tool if selected layer doesn't change.
                -- Set the same type of tool that was previously in use if layer changes.
                Just _ ->
                    { state
                        | selectedLayerIndexAndTool = Just ( i, Pan )
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
    case state.selectedLayerIndexAndTool of
        Just ( selectedIndex, _ ) ->
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
            , mapPaneView state
            ]
                ++ (modalView <| getModal state)
    in
    Html.div [ Html.Attributes.class "page" ]
        content



-- MODAL


modalView : Maybe Modal -> List (Html Msg)
modalView modal =
    case modal of
        Just m ->
            [ Html.div [ Html.Attributes.class "mask" ]
                [ Html.div [ Html.Attributes.class "modal" ]
                    [ modalContent m
                    ]
                ]
            ]

        _ ->
            []


modalContent : Modal -> Html Msg
modalContent modal =
    case modal of
        NewLayerDialog layerName ->
            newLayerDialog layerName

        GradientEditorDialog state ->
            gradientEditorDialog state


newLayerDialog : String -> Html Msg
newLayerDialog layerName =
    Html.div [ Html.Attributes.class "new-layer-dialog" ]
        [ Html.div [ Html.Attributes.class "new-layer-dialog__header" ] [ Html.text "New Layer" ]
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



-- MAP PANE


mapPaneView : State -> Html Msg
mapPaneView state =
    Html.div [ Html.Attributes.class "map-pane" ]
        []



-- TOOLBAR


toolbar : State -> Html Msg
toolbar state =
    let
        ( layerIndex, layer, tool ) =
            mapTuple3 <| getSelectedLayerAndTool state
    in
    Html.div [ Html.Attributes.class "toolbar" ]
        [ toolbarSection
            [ toolbarHeader "Layer"
            , layerField layerIndex state.layers
            , minMaxField layer
            , colorGradientField layer
            ]
        , toolbarSection
            [ toolbarHeader "Tools"
            ]
        , toolbarSection
            [ toolbarHeader "Brush"
            ]
        ]


toolbarSection : List (Html Msg) -> Html Msg
toolbarSection contents =
    Html.div [ Html.Attributes.class "toolbar__section" ]
        contents


toolbarHeader : String -> Html Msg
toolbarHeader header =
    Html.div [ Html.Attributes.class "toolbar__header" ]
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
-- HELPERS


isJustEqual : Maybe a -> a -> Bool
isJustEqual maybeX y =
    case maybeX of
        Nothing ->
            False

        Just x ->
            x == y


mapTuple3 : Maybe ( a, b, c ) -> ( Maybe a, Maybe b, Maybe c )
mapTuple3 tuple =
    case tuple of
        Just ( x, y, z ) ->
            ( Just x, Just y, Just z )

        Nothing ->
            ( Nothing, Nothing, Nothing )


{-| Remove the element at the given index in the list.
-}
remove : Int -> List a -> List a
remove index xs =
    List.take index xs ++ List.drop (index + 1) xs
