module DiscreteGradientEditor exposing (Msg, Output(..), State, init, update, view)

import Color exposing (Color)
import ColorPicker
import DiscreteGradient
    exposing
        ( ColorStop
        , DiscreteGradient
        , addStop
        , getColorAt
        , getStops
        , removeStopAt
        )
import Hex
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, classList, disabled, style, type_, value)
import Html.Events exposing (onClick, onInput)



-- MODEL


type State
    = State Model


init : DiscreteGradient -> Int -> Int -> State
init dg gradientMin gradientMax =
    State (initModel dg gradientMin gradientMax)


type alias Model =
    { gradient : DiscreteGradient
    , gradientMin : Int -- Minimum value on the gradient where a stop can be defined
    , gradientMax : Int -- Maximum value on the gradient where a stop can be defined
    , selectedValue : Int
    , selectedColor : Color
    , colorPickerState : ColorPicker.State
    }


initModel : DiscreteGradient -> Int -> Int -> Model
initModel dg gradientMin gradientMax =
    { gradientMin = gradientMin
    , gradientMax = gradientMax
    , gradient = dg
    , selectedValue = gradientMin
    , selectedColor = DiscreteGradient.getColorAt gradientMin dg
    , colorPickerState = ColorPicker.empty
    }



-- UPDATE


type Msg
    = ClickGradientCell Int
    | ColorPickerMsg ColorPicker.Msg
    | ChangeRed String
    | ChangeGreen String
    | ChangeBlue String
    | ChangeHue String
    | ChangeSaturation String
    | ChangeLightness String
    | ChangeHex String
    | ChangeStopValue String
    | ClickCreateStop
    | ClickDeleteStop
    | ClickCancel
    | ClickSave


type Output
    = EditInProgress
    | Cancel
    | Save DiscreteGradient


update : Msg -> State -> ( State, Output )
update msg (State model) =
    update_ msg model
        |> Tuple.mapFirst State


update_ : Msg -> Model -> ( Model, Output )
update_ msg model =
    case msg of
        ClickGradientCell cellVal ->
            selectGradientVal model cellVal
                |> andEditInProgress

        ColorPickerMsg cpMsg ->
            if hasStopAt model.selectedValue model.gradient then
                let
                    ( colorPicker, maybeColor ) =
                        ColorPicker.update cpMsg model.selectedColor model.colorPickerState

                    newColor =
                        Maybe.withDefault model.selectedColor maybeColor
                in
                { model
                    | gradient = DiscreteGradient.addStop { value = model.selectedValue, color = newColor } model.gradient
                    , selectedColor = newColor
                    , colorPickerState = colorPicker
                }
                    |> andEditInProgress

            else
                model
                    |> andEditInProgress

        ChangeRed red ->
            updateSelectedStopColorChannel updateRedChannel red model
                |> andEditInProgress

        ChangeGreen green ->
            updateSelectedStopColorChannel updateGreenChannel green model
                |> andEditInProgress

        ChangeBlue blue ->
            updateSelectedStopColorChannel updateBlueChannel blue model
                |> andEditInProgress

        ChangeHue hue ->
            updateSelectedStopColorChannel updateHueChannel hue model
                |> andEditInProgress

        ChangeSaturation saturation ->
            updateSelectedStopColorChannel updateSaturationChannel saturation model
                |> andEditInProgress

        ChangeLightness lightness ->
            updateSelectedStopColorChannel updateLightnessChannel lightness model
                |> andEditInProgress

        ChangeHex hex ->
            hex
                |> colorFromRgbHex
                |> Maybe.map (setSelectedStopColor model)
                |> Maybe.withDefault model
                |> andEditInProgress

        ChangeStopValue stopVal ->
            stopVal
                |> String.toInt
                |> Maybe.map (selectGradientVal model)
                |> Maybe.withDefault model
                |> andEditInProgress

        ClickCreateStop ->
            let
                colorAtSelectedVal =
                    DiscreteGradient.getColorAt model.selectedValue model.gradient

                newStop =
                    { value = model.selectedValue, color = colorAtSelectedVal }
            in
            { model
                | gradient = DiscreteGradient.addStop newStop model.gradient
            }
                |> andEditInProgress

        ClickDeleteStop ->
            { model
                | gradient = DiscreteGradient.removeStopAt model.selectedValue model.gradient
            }
                |> resetSelectedColor
                |> andEditInProgress

        ClickCancel ->
            ( model, Cancel )

        ClickSave ->
            ( model, Save model.gradient )


andEditInProgress : a -> ( a, Output )
andEditInProgress a =
    ( a, EditInProgress )


selectGradientVal : Model -> Int -> Model
selectGradientVal model val =
    let
        nextGradientVal =
            boundInt model.gradientMin model.gradientMax val
    in
    { model
        | selectedValue = nextGradientVal
        , selectedColor = DiscreteGradient.getColorAt nextGradientVal model.gradient
        , colorPickerState = ColorPicker.empty
    }


{-| Reset the selected color to the color on the gradient at the currently selected value.
-}
resetSelectedColor : Model -> Model
resetSelectedColor model =
    { model
        | selectedColor = DiscreteGradient.getColorAt model.selectedValue model.gradient
    }


{-| Update a color channel of the stop at the selected gradient value if a stop exists there.
-}
updateSelectedStopColorChannel : (Color -> Int -> Color) -> String -> Model -> Model
updateSelectedStopColorChannel updateChannel newChannelVal model =
    newChannelVal
        |> String.toInt
        |> Maybe.map (updateChannel model.selectedColor)
        |> Maybe.map (setSelectedStopColor model)
        |> Maybe.withDefault model


{-| Set the color of the stop at the selected gradient value if a stop exists there.
-}
setSelectedStopColor : Model -> Color -> Model
setSelectedStopColor model color =
    if hasStopAt model.selectedValue model.gradient then
        { model
            | gradient = DiscreteGradient.addStop { value = model.selectedValue, color = color } model.gradient
            , selectedColor = color
        }

    else
        model


updateRedChannel : Color -> Int -> Color
updateRedChannel color red =
    let
        rgba =
            Color.toRgba color
    in
    Color.fromRgba
        { rgba
            | red = floatFrom255 <| red
        }


updateGreenChannel : Color -> Int -> Color
updateGreenChannel color green =
    let
        rgba =
            Color.toRgba color
    in
    Color.fromRgba
        { rgba
            | green = floatFrom255 <| green
        }


updateBlueChannel : Color -> Int -> Color
updateBlueChannel color blue =
    let
        rgba =
            Color.toRgba color
    in
    Color.fromRgba
        { rgba
            | blue = floatFrom255 <| blue
        }


updateHueChannel : Color -> Int -> Color
updateHueChannel color hue =
    let
        hsl =
            Color.toHsla color
    in
    Color.fromHsla
        { hsl
            | hue = floatFrom255 <| hue
        }


updateSaturationChannel : Color -> Int -> Color
updateSaturationChannel color saturation =
    let
        hsl =
            Color.toHsla color
    in
    Color.fromHsla
        { hsl
            | saturation = floatFrom255 <| saturation
        }


updateLightnessChannel : Color -> Int -> Color
updateLightnessChannel color lightness =
    let
        hsl =
            Color.toHsla color
    in
    Color.fromHsla
        { hsl
            | lightness = floatFrom255 <| lightness
        }



-- VIEW


view : State -> Html Msg
view (State model) =
    let
        isStopAtSelectedValue =
            hasStopAt model.selectedValue model.gradient
    in
    div [ class "gradient-editor" ]
        [ div [ class "gradient-editor__header" ] [ text "Edit Gradient" ]
        , div [ class "gradient-editor__gradient" ] [ gradientView model.gradient model.gradientMin model.gradientMax ]
        , div [ class "gradient-editor__color-and-stop-row" ]
            [ colorSwatchView model.selectedColor
            , colorPickerView model.selectedColor model.colorPickerState
            , div [ class "gradient-editor__color-column" ]
                [ div [ class "gradient-editor__rgb-hsl-row" ]
                    [ rgbColumnView model.selectedColor (not isStopAtSelectedValue)
                    , hslColumnView model.selectedColor (not isStopAtSelectedValue)
                    ]
                , hexRowView model.selectedColor (not isStopAtSelectedValue)
                ]
            , stopColumnView model.gradient model.selectedValue
            ]
        , div [ class "gradient-editor__close-button-row" ]
            [ button [ class "gradient-editor__close-button", onClick ClickCancel ] [ text "Cancel" ]
            , button [ class "gradient-editor__close-button", onClick ClickSave ] [ text "Save" ]
            ]
        ]


gradientView : DiscreteGradient -> Int -> Int -> Html Msg
gradientView dg gradientMin gradientMax =
    let
        cellViews =
            List.map (cellView dg) (List.range gradientMin gradientMax)
    in
    div [ class "gradient" ]
        [ div [ class "gradient__row" ]
            cellViews
        ]


cellView : DiscreteGradient -> Int -> Html Msg
cellView dg val =
    let
        color =
            DiscreteGradient.getColorAt val dg
    in
    div
        [ classList
            [ ( "gradient__cell", True )
            , ( "gradient__cell_has-stop", hasStopAt val dg )
            ]
        , style "background-color" (Color.toCssString color)
        , onClick (ClickGradientCell val)
        ]
        [ div [ class "tooltip" ]
            [ text (String.fromInt val)
            , div [ class "tooltip__triangle" ] []
            ]
        ]


colorSwatchView : Color -> Html Msg
colorSwatchView color =
    div
        [ class "gradient-editor__color-swatch"
        , style "background-color" (Color.toCssString color)
        ]
        []


colorPickerView : Color -> ColorPicker.State -> Html Msg
colorPickerView color colorPicker =
    div [ class "gradient-editor__color-picker" ]
        [ ColorPicker.view color colorPicker
            |> Html.map ColorPickerMsg
        ]


rgbColumnView : Color -> Bool -> Html Msg
rgbColumnView color isDisabled =
    let
        ( red, green, blue ) =
            toRgb255 color
    in
    div [ class "gradient-editor__rgb-column" ]
        [ div [ class "gradient-editor__column-header gradient-editor__column-item" ] [ text "RGB" ]
        , input
            [ class "gradient-editor__number-input"
            , class "gradient-editor__column-item"
            , type_ "number"
            , disabled isDisabled
            , value <| String.fromInt red
            , onInput ChangeRed
            ]
            []
        , input
            [ class "gradient-editor__number-input"
            , class "gradient-editor__column-item"
            , type_ "number"
            , disabled isDisabled
            , value <| String.fromInt green
            , onInput ChangeGreen
            ]
            []
        , input
            [ class "gradient-editor__number-input"
            , class "gradient-editor__column-item"
            , type_ "number"
            , disabled isDisabled
            , value <| String.fromInt blue
            , onInput ChangeBlue
            ]
            []
        ]


hslColumnView : Color -> Bool -> Html Msg
hslColumnView color isDisabled =
    let
        ( hue, saturation, lightness ) =
            toHsl255 color
    in
    div [ class "gradient-editor__hsl-column" ]
        [ div [ class "gradient-editor__column-header gradient-editor__column-item" ] [ text "HSL" ]
        , input
            [ class "gradient-editor__number-input"
            , class "gradient-editor__column-item"
            , type_ "number"
            , disabled isDisabled
            , value <| String.fromInt hue
            , onInput ChangeHue
            ]
            []
        , input
            [ class "gradient-editor__number-input"
            , class "gradient-editor__column-item"
            , type_ "number"
            , disabled isDisabled
            , value <| String.fromInt saturation
            , onInput ChangeSaturation
            ]
            []
        , input
            [ class "gradient-editor__number-input"
            , class "gradient-editor__column-item"
            , type_ "number"
            , disabled isDisabled
            , value <| String.fromInt lightness
            , onInput ChangeLightness
            ]
            []
        ]


hexRowView : Color -> Bool -> Html Msg
hexRowView color isDisabled =
    div [ class "gradient-editor__hex-row" ]
        [ div [ class "gradient-editor__hex-label" ] [ text "Hex #" ]
        , input
            [ class "gradient-editor__hex-input"
            , disabled isDisabled
            , value <| toRgbHex color
            , onInput ChangeHex
            ]
            []
        ]


stopColumnView : DiscreteGradient -> Int -> Html Msg
stopColumnView dg gradientVal =
    let
        hasStop =
            hasStopAt gradientVal dg

        stopCount =
            List.length <| DiscreteGradient.getStops dg
    in
    div [ class "gradient-editor__stop-column" ]
        [ div [ class "gradient-editor__column-header gradient-editor__column-item" ] [ text "Stop" ]
        , input
            [ class "gradient-editor__number-input"
            , class "gradient-editor__column-item"
            , type_ "number"
            , value <| String.fromInt gradientVal
            , onInput ChangeStopValue
            ]
            []
        , button
            [ class "gradient-editor__column-item"
            , disabled hasStop -- enabled iff cell does not have stop
            , onClick ClickCreateStop
            ]
            [ text "Create" ]
        , button
            [ class "gradient-editor__column-item"
            , disabled <| not hasStop || stopCount <= 1 -- enabled iff cell has stop and gradient has more than 1 stop
            , onClick ClickDeleteStop
            ]
            [ text "Delete" ]
        ]



-- HELPERS


{-| If the given int is less than the lower bound return the lower bound. If the given int is greater than the upper
bound return the upper bound. Lower bound must be less than or equal to the upper bound.
-}
boundInt : Int -> Int -> Int -> Int
boundInt lowerBound upperBound =
    min upperBound << max lowerBound


toRgb255 : Color -> ( Int, Int, Int )
toRgb255 color =
    let
        rgba =
            Color.toRgba color
    in
    ( rgba.red |> floatTo255
    , rgba.green |> floatTo255
    , rgba.blue |> floatTo255
    )


toHsl255 : Color -> ( Int, Int, Int )
toHsl255 color =
    let
        hsla =
            Color.toHsla color
    in
    ( hsla.hue |> floatTo255
    , hsla.saturation |> floatTo255
    , hsla.lightness |> floatTo255
    )


toRgbHex : Color -> String
toRgbHex color =
    let
        ( red, green, blue ) =
            toRgb255 color

        redHex =
            red |> Hex.toString |> String.padLeft 2 '0'

        greenHex =
            green |> Hex.toString |> String.padLeft 2 '0'

        blueHex =
            blue |> Hex.toString |> String.padLeft 2 '0'
    in
    redHex ++ greenHex ++ blueHex


colorFromRgbHex : String -> Maybe Color
colorFromRgbHex hex =
    if String.length hex == 6 then
        let
            maybeR =
                hex |> String.slice 0 2 |> Hex.fromString |> Result.toMaybe

            maybeG =
                hex |> String.slice 2 4 |> Hex.fromString |> Result.toMaybe

            maybeB =
                hex |> String.slice 4 6 |> Hex.fromString |> Result.toMaybe
        in
        case ( maybeR, maybeG, maybeB ) of
            ( Just r, Just g, Just b ) ->
                tryColorFromRgb255 r g b

            _ ->
                Nothing

    else
        Nothing


floatTo255 : Float -> Int
floatTo255 f =
    if f < 0.0 then
        0

    else if f > 1.0 then
        255

    else
        round <| f * 255


floatFrom255 : Int -> Float
floatFrom255 i =
    if i < 0 then
        0.0

    else if i > 255 then
        1.0

    else
        toFloat i / 255.0


hasStopAt : Int -> DiscreteGradient -> Bool
hasStopAt val dg =
    dg
        |> DiscreteGradient.getStops
        |> List.map (\s -> s.value)
        |> List.member val


tryColorFromRgb255 : Int -> Int -> Int -> Maybe Color
tryColorFromRgb255 r g b =
    if
        (r >= 0)
            && (r <= 255)
            && (g >= 0)
            && (g <= 255)
            && (b >= 0)
            && (b <= 255)
    then
        Just <|
            Color.fromRgba
                { red = floatFrom255 r
                , green = floatFrom255 g
                , blue = floatFrom255 b
                , alpha = 1.0
                }

    else
        Nothing
