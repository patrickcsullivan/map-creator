module View exposing (view)

import Color exposing (toCssString)
import DiscreteGradient exposing (DiscreteGradient)
import DiscreteGradientEditor
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, classList, disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Layer exposing (Layer)
import Maybe.Extra exposing (isNothing, toList)
import Model exposing (Modal(..), Model)
import Update exposing (Msg(..), toSelectLayerMsg, toSetLayerMaxMsg, toSetLayerMinMsg)


view : Model -> Html Msg
view model =
    let
        content =
            [ toolbar model ] ++ (modalView <| Model.getModal model)
    in
    div [ class "page" ]
        content



-- MODAL


modalView : Maybe Modal -> List (Html Msg)
modalView modal =
    case modal of
        Just m ->
            [ div [ class "mask" ]
                [ div [ class "modal" ]
                    [ modalContent m
                    ]
                ]
            ]

        _ ->
            []


modalContent : Modal -> Html Msg
modalContent modal =
    case modal of
        Model.NewLayerDialog layerName ->
            newLayerDialog layerName

        Model.GradientEditorDialog state ->
            gradientEditorDialog state


newLayerDialog : String -> Html Msg
newLayerDialog layerName =
    div [ class "new-layer-dialog" ]
        [ div [ class "new-layer-dialog__header" ] [ text "New Layer" ]
        , div [ class "new-layer-dialog__name-field" ]
            [ div [ class "new-layer-dialog__name-label" ] [ text "Name" ]
            , input
                [ class "new-layer-dialog__name-label"
                , value layerName
                , onInput SetNewLayerDialogNameField
                ]
                []
            ]
        , div [ class "new-layer-dialog__button-container" ]
            [ button
                [ class "new-layer-dialog__button"
                , onClick NewLayerDialogCancel
                ]
                [ text "Cancel" ]
            , button
                [ class "new-layer-dialog__button"
                , onClick NewLayerDialogCreate
                ]
                [ text "Create" ]
            ]
        ]


gradientEditorDialog : DiscreteGradientEditor.State -> Html Msg
gradientEditorDialog state =
    DiscreteGradientEditor.view state
        |> Html.map Update.GradientEditorMsg



-- TOOLBAR


toolbar : Model -> Html Msg
toolbar model =
    let
        ( layerIndex, layer, tool ) =
            mapTuple3 <| Model.getSelectedLayerAndTool model
    in
    div [ class "toolbar" ]
        [ toolbarSection
            [ toolbarHeader "Layer"
            , layerField layerIndex (Model.getLayers model)
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
    div [ class "toolbar__section" ]
        contents


toolbarHeader : String -> Html Msg
toolbarHeader header =
    div [ class "toolbar__header" ]
        [ text header
        ]



-- LAYER FIELD


layerField : Maybe Int -> List Layer -> Html Msg
layerField selectedIndex layers =
    div
        [ class "layer-field"
        ]
        [ layerFieldSelect selectedIndex layers
        , layerFieldNewButton
        , layerFieldDeleteButton
        ]


layerFieldSelect : Maybe Int -> List Layer -> Html Msg
layerFieldSelect selectedIndex layers =
    let
        emptyOption =
            layerFieldEmptyOption (isNothing selectedIndex)

        options =
            [ emptyOption ] ++ layerFieldOptions selectedIndex layers
    in
    Html.select
        [ class "layer-field__select"
        , onInput toSelectLayerMsg
        ]
        options


layerFieldEmptyOption : Bool -> Html Msg
layerFieldEmptyOption isSelected =
    Html.option
        [ Html.Attributes.value ""
        , Html.Attributes.selected isSelected
        ]
        [ text ""
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
        [ text <| Layer.getName layer
        ]


layerFieldNewButton : Html Msg
layerFieldNewButton =
    button [ onClick OpenNewLayerDialog ] [ text "+" ]


layerFieldDeleteButton : Html Msg
layerFieldDeleteButton =
    button [ onClick DeleteSelectedLayer ] [ text "-" ]



-- COLOR FIELD


colorGradientField : Maybe Layer -> Html Msg
colorGradientField layer =
    div [ class "color-field" ]
        [ colorFieldLabel
        , colorGradientRow layer
        ]


colorFieldLabel : Html Msg
colorFieldLabel =
    div [ class "color-field__label" ]
        [ text "Color"
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
    div [ class "color-gradient__row" ]
        contents


enabledColorGradient : DiscreteGradient -> Int -> Int -> Html Msg
enabledColorGradient gradient layerMin layerMax =
    div
        [ class "color-gradient"
        ]
        [ simpleGradient gradient layerMin layerMax
        ]


disabledColorGradient : Html Msg
disabledColorGradient =
    div
        [ class "color-gradient"
        , class "color-gradient_disabled"
        ]
        []


colorGradientEditButton : Bool -> Html Msg
colorGradientEditButton isDisabled =
    button
        [ disabled isDisabled
        , onClick OpenGradientEditorDialog
        ]
        [ text "Edit"
        ]


simpleGradient : DiscreteGradient -> Int -> Int -> Html Msg
simpleGradient gradient layerMin layerMax =
    let
        cells =
            List.map (simpleGradientCell gradient) (List.range layerMin layerMax)
    in
    div [ class "simple-gradient" ]
        [ div [ class "simple-gradient__row" ]
            cells
        ]


simpleGradientCell : DiscreteGradient -> Int -> Html Msg
simpleGradientCell gradient val =
    let
        color =
            DiscreteGradient.getColorAt val gradient
    in
    div
        [ class "simple-gradient__cell"
        , style "background-color" (Color.toCssString color)
        ]
        []



-- MIN / MAX CONTROL


minMaxField : Maybe Layer -> Html Msg
minMaxField layer =
    div [ class "min-max-field" ]
        [ minMaxFieldLabel
        , minInput <| Maybe.map Layer.getMin layer
        , minMaxFieldInputSeparator
        , maxInput <| Maybe.map Layer.getMax layer
        ]


minMaxFieldLabel : Html Msg
minMaxFieldLabel =
    div [ class "min-max-field__label" ]
        [ text "Min / Max"
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
    input
        [ class "min-max-field__input"
        , Html.Attributes.type_ "number"
        , Html.Attributes.value <| String.fromInt val
        , Html.Events.onInput toMsg
        ]
        []


disabledMinMaxFieldInput : Html Msg
disabledMinMaxFieldInput =
    input
        [ class "min-max-field__input"
        , Html.Attributes.type_ "number"
        , Html.Attributes.disabled True
        ]
        []


minMaxFieldInputSeparator : Html Msg
minMaxFieldInputSeparator =
    div
        [ class "min-max-field__input-separator"
        ]
        [ text "/"
        ]



-- HELPERS


isJustEqual : Maybe a -> a -> Bool
isJustEqual maybeX y =
    case maybeX of
        Nothing ->
            False

        Just x ->
            x == y


onClick : msg -> Html.Attribute msg
onClick message =
    Html.Events.on "click" (Decode.succeed message)


mapTuple3 : Maybe ( a, b, c ) -> ( Maybe a, Maybe b, Maybe c )
mapTuple3 tuple =
    case tuple of
        Just ( x, y, z ) ->
            ( Just x, Just y, Just z )

        Nothing ->
            ( Nothing, Nothing, Nothing )
