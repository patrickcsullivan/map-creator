module Layer exposing
    ( Layer
    , addColorStop
    , getColorGradient
    , getGrid
    , getHeight
    , getMax
    , getMin
    , getName
    , getWidth
    , init
    , removeColorStopAt
    , resizeGrid
    , setAtGridCoordinate
    , setMax
    , setMin
    , setName
    )

import Color exposing (Color)
import ColorGradient
    exposing
        ( ColorGradient
        , ColorStop
        , addStop
        , getColorAt
        , getStops
        , removeStopAt
        )
import Grid exposing (Coordinate, Grid)



-- INVARIANTS:
-- Grid width must be greater than 0.
-- Grid height must be greater than 0.
-- Min is always less than or equal to max.
-- Grid cells must each be in the range [min, max].
-- All stops in color gradient have values in the range [min, max].
-- Changing min or max may not change the calculated colors in the color gradient within then new min-max range, but stops may be added or removed.


type Layer
    = Layer InnerLayer


type alias InnerLayer =
    { name : String
    , width : Int
    , height : Int
    , min : Int
    , max : Int
    , grid : Grid Int
    , colorGradient : ColorGradient
    }


init : String -> Int -> Int -> Int -> Int -> Layer
init name width height minVal maxVal =
    Layer
        { name = name
        , width = max width 1
        , height = max height 1
        , min = minVal
        , max = maxVal
        , colorGradient = ColorGradient.init { value = minVal, color = defaultColor }
        , grid = Grid.repeat width height minVal
        }


getName : Layer -> String
getName (Layer inner) =
    inner.name


getMin : Layer -> Int
getMin (Layer inner) =
    inner.min


getMax : Layer -> Int
getMax (Layer inner) =
    inner.max


getWidth : Layer -> Int
getWidth (Layer inner) =
    inner.width


getHeight : Layer -> Int
getHeight (Layer inner) =
    inner.height


getColorGradient : Layer -> ColorGradient
getColorGradient (Layer inner) =
    inner.colorGradient


getGrid : Layer -> Grid Int
getGrid (Layer inner) =
    inner.grid


setName : String -> Layer -> Layer
setName name (Layer inner) =
    Layer { inner | name = name }


setMin : Int -> Layer -> Layer
setMin newMin (Layer inner) =
    if newMin > inner.max || newMin == inner.min then
        Layer inner

    else if newMin < inner.min then
        Layer (setDecreasedMin newMin inner)

    else
        Layer (setIncreasedMin newMin inner)


setMax : Int -> Layer -> Layer
setMax newMax (Layer inner) =
    if newMax < inner.min || newMax == inner.max then
        Layer inner

    else if newMax < inner.max then
        Layer (setDecreasedMax newMax inner)

    else
        Layer (setIncreasedMax newMax inner)


resizeGrid : Int -> Int -> Layer -> Layer
resizeGrid width height (Layer inner) =
    if width < 1 || height < 1 || (width == inner.width && height == inner.height) then
        Layer inner

    else
        Layer
            { inner
                | grid = unsafeResizeGrid width height inner.min inner.grid
            }


setAtGridCoordinate : Coordinate -> Int -> Layer -> Layer
setAtGridCoordinate coord value (Layer inner) =
    if value >= inner.min && value <= inner.max then
        Layer
            { inner
                | grid = inner.grid |> Grid.set coord value
            }

    else
        Layer inner


addColorStop : ColorStop -> Layer -> Layer
addColorStop stop (Layer inner) =
    if stop.value >= inner.min && stop.value <= inner.max then
        Layer
            { inner
                | colorGradient = addStop stop inner.colorGradient
            }

    else
        Layer inner


removeColorStopAt : Int -> Layer -> Layer
removeColorStopAt value (Layer inner) =
    Layer
        { inner
            | colorGradient = removeStopAt value inner.colorGradient
        }



-- UNEXPOSED


defaultColor : Color
defaultColor =
    Color.lightGray


unsafeResizeGrid : Int -> Int -> a -> Grid a -> Grid a
unsafeResizeGrid width height default grid =
    let
        filler : Int -> Int -> a
        filler x y =
            case Grid.get ( x, y ) grid of
                Just cell ->
                    cell

                Nothing ->
                    default
    in
    Grid.rectangle width height filler


setDecreasedMin : Int -> InnerLayer -> InnerLayer
setDecreasedMin newMin inner =
    { inner | min = newMin }


setIncreasedMin : Int -> InnerLayer -> InnerLayer
setIncreasedMin newMin inner =
    { inner
        | min = newMin
        , colorGradient = applyFloorToColorStops newMin inner.colorGradient
        , grid = applyFloorToGrid newMin inner.grid
    }


setIncreasedMax : Int -> InnerLayer -> InnerLayer
setIncreasedMax newMax inner =
    { inner | max = newMax }


setDecreasedMax : Int -> InnerLayer -> InnerLayer
setDecreasedMax newMax inner =
    { inner
        | max = newMax
        , colorGradient = applyCeilingToColorStops newMax inner.colorGradient
        , grid = applyCeilingToGrid newMax inner.grid
    }


{-| Remove all color stops with values below the floor and create a color stop
at the floor so that gradient colors above the floor are preserved.
-}
applyFloorToColorStops : Int -> ColorGradient -> ColorGradient
applyFloorToColorStops flr gradient =
    let
        colorAtFloor =
            getColorAt flr gradient

        stopsBelowFloor =
            gradient |> getStops |> List.filter (\s -> s.value < flr)
    in
    gradient
        |> removeColorStops stopsBelowFloor
        |> addStop { value = flr, color = colorAtFloor }


{-| Remove all color stops with values above the ceiling and create a color stop
at the ceiling so that gradient colors below the ceiling are preserved.
-}
applyCeilingToColorStops : Int -> ColorGradient -> ColorGradient
applyCeilingToColorStops ceil gradient =
    let
        colorAtCeiling =
            getColorAt ceil gradient

        stopsAboveCeiling =
            gradient |> getStops |> List.filter (\s -> s.value > ceil)
    in
    gradient
        |> removeColorStops stopsAboveCeiling
        |> addStop { value = ceil, color = colorAtCeiling }


{-| Remove the color stops from gradient.
-}
removeColorStops : List ColorStop -> ColorGradient -> ColorGradient
removeColorStops stops gradient =
    stops
        -- Map to list of stop values.
        |> List.map (\s -> s.value)
        -- Map to list of functions that each removes a stop.
        |> List.map removeStopAt
        -- Apply each function to remove all stops.
        |> List.foldl (<|) gradient


{-| Increase any values in grid that are below the floor.
-}
applyFloorToGrid : Int -> Grid Int -> Grid Int
applyFloorToGrid flr =
    Grid.map (max flr)


{-| Decrease any values in grid that are above the ceiling.
-}
applyCeilingToGrid : Int -> Grid Int -> Grid Int
applyCeilingToGrid ceil =
    Grid.map (min ceil)
