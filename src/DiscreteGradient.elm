module DiscreteGradient exposing
    ( ColorStop
    , DiscreteGradient
    , addStop
    , getColorAt
    , getStops
    , init
    , removeStopAt
    )

import Color exposing (Color)



-- INVARIANTS:
-- List of color stops most stay ordered by value.
-- List of color stops may not be empty.
-- List of color stops may not contain more than one stop with the same value.


{-| Gradient of colors defined by color stops at integer values
-}
type DiscreteGradient
    = DiscreteGradient (List ColorStop)


{-| Value on a color gradient at which a color is defined
-}
type alias ColorStop =
    { value : Int
    , color : Color
    }


{-| Create gradient with a single color stop.
-}
init : ColorStop -> DiscreteGradient
init stop =
    DiscreteGradient [ stop ]


{-| Get the list of color stops, ordered by increasing value.
-}
getStops : DiscreteGradient -> List ColorStop
getStops (DiscreteGradient stops) =
    stops


{-| Add a color stop. If a color stop with the same value exists, replace it.
-}
addStop : ColorStop -> DiscreteGradient -> DiscreteGradient
addStop stop (DiscreteGradient stops) =
    DiscreteGradient
        (stopsBelow stop.value stops
            ++ [ stop ]
            ++ stopsAbove stop.value stops
        )


{-| Remove a color stop at a given value. Do nothing if there is only one color
stop.
-}
removeStopAt : Int -> DiscreteGradient -> DiscreteGradient
removeStopAt value (DiscreteGradient stops) =
    if List.length stops > 1 then
        DiscreteGradient (stopsBelow value stops ++ stopsAbove value stops)

    else
        DiscreteGradient stops


{-| Get a color at a value on the gradient.
-}
getColorAt : Int -> DiscreteGradient -> Color
getColorAt value gradient =
    let
        maybeLowerStop =
            nearestStopAtOrUnder value gradient

        maybeUpperStop =
            nearestStopAtOrAbove value gradient
    in
    case ( maybeLowerStop, maybeUpperStop ) of
        ( Just lower, Just upper ) ->
            interpolateBetween lower upper value

        ( Just lower, Nothing ) ->
            lower.color

        ( Nothing, Just upper ) ->
            upper.color

        ( Nothing, Nothing ) ->
            Color.black


stopsBelow : Int -> List ColorStop -> List ColorStop
stopsBelow value =
    List.filter (\s -> s.value < value)


stopsAbove : Int -> List ColorStop -> List ColorStop
stopsAbove value =
    List.filter (\s -> s.value > value)



-- TODO: Optimize finding the neareast above and nearest under stops.


nearestStopAtOrUnder : Int -> DiscreteGradient -> Maybe ColorStop
nearestStopAtOrUnder value (DiscreteGradient stops) =
    stops
        |> List.filter (\s -> s.value <= value)
        |> List.reverse
        |> List.head


nearestStopAtOrAbove : Int -> DiscreteGradient -> Maybe ColorStop
nearestStopAtOrAbove value (DiscreteGradient stops) =
    stops
        |> List.filter (\s -> s.value >= value)
        |> List.head


{-| Interpolate a color at a value between the lower and upper color stops.
-}
interpolateBetween : ColorStop -> ColorStop -> Int -> Color
interpolateBetween lower upper value =
    if lower == upper then
        lower.color

    else
        let
            factor =
                fractionBetween lower.value upper.value value
        in
        interpolateColor lower.color upper.color factor


{-| Interpolate between two colors by a factor in the range [0,1].
-}
interpolateColor : Color -> Color -> Float -> Color
interpolateColor =
    interpolateRgba



-- TODO: Pick an interpolation mode or allow picking via parameter.


interpolateRgba : Color -> Color -> Float -> Color
interpolateRgba lower upper factor =
    let
        lowerRgba =
            Color.toRgba lower

        upperRgba =
            Color.toRgba upper
    in
    Color.fromRgba
        { red = interpolateFloat lowerRgba.red upperRgba.red factor
        , green = interpolateFloat lowerRgba.green upperRgba.green factor
        , blue = interpolateFloat lowerRgba.blue upperRgba.blue factor
        , alpha = interpolateFloat lowerRgba.alpha upperRgba.alpha factor
        }


interpolateHsla : Color -> Color -> Float -> Color
interpolateHsla lower upper factor =
    let
        lowerHsla =
            Color.toHsla lower

        upperHsla =
            Color.toHsla upper
    in
    Color.fromHsla
        { hue = interpolateFloat lowerHsla.hue upperHsla.hue factor
        , saturation = interpolateFloat lowerHsla.saturation upperHsla.saturation factor
        , lightness = interpolateFloat lowerHsla.lightness upperHsla.lightness factor
        , alpha = interpolateFloat lowerHsla.alpha upperHsla.alpha factor
        }


{-| Interpolate between two floats by a factor in the range [0,1].
-}
interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat lower upper factor =
    lower + (upper - lower) * factor


fractionBetween : Int -> Int -> Int -> Float
fractionBetween lower upper x =
    let
        lowerF =
            toFloat lower

        upperF =
            toFloat upper

        xF =
            toFloat x
    in
    (xF - lowerF) / (upperF - lowerF)
