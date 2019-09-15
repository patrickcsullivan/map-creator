module Layer exposing
    ( Layer
    , getColorGradient
    , getGrid
    , getHeight
    , getMax
    , getMin
    , getName
    , getWidth
    , init
    , resizeGrid
    , setAtGridCoordinate
    , setColorGradient
    , setGrid
    , setMax
    , setMin
    , setName
    )

import Color exposing (Color)
import DiscreteGradient
    exposing
        ( ColorStop
        , DiscreteGradient
        , addStop
        , getColorAt
        , getStops
        , removeStopAt
        )
import Grid exposing (Coordinate, Grid)



-- TODO:
-- Refactor this. Maybe it doesn't need to be a closed type. There are a lot of getters and setters and many setters
-- aren't being used.
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
    , colorGradient : DiscreteGradient
    }


init : String -> Int -> Int -> Int -> Int -> Layer
init name width height minVal maxVal =
    Layer
        { name = name
        , width = max width 1
        , height = max height 1
        , min = minVal
        , max = maxVal
        , colorGradient = DiscreteGradient.init { value = minVal, color = defaultColor }
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


getColorGradient : Layer -> DiscreteGradient
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


setGrid : Grid Int -> Layer -> Layer
setGrid grid (Layer inner) =
    let
        newGrid =
            grid
                |> resizeGrid_ inner.min inner.width inner.height
                |> applyFloorToGrid inner.min
                |> applyCeilingToGrid inner.max
    in
    Layer
        { inner
            | grid = grid
        }


setColorGradient : DiscreteGradient -> Layer -> Layer
setColorGradient dg (Layer inner) =
    let
        newGradient =
            dg
                |> applyFloorToColorStops inner.min
                |> applyCeilingToColorStops inner.max
    in
    Layer
        { inner
            | colorGradient = newGradient
        }


resizeGrid : Int -> Int -> Layer -> Layer
resizeGrid width height (Layer inner) =
    Layer
        { inner
            | grid = resizeGrid_ inner.min width height inner.grid
        }


resizeGrid_ : Int -> Int -> Int -> Grid Int -> Grid Int
resizeGrid_ default width height grid =
    if width < 1 || height < 1 || (width == Grid.width grid) && (height == Grid.height grid) then
        grid

    else
        let
            filler : Int -> Int -> Int
            filler x y =
                case Grid.get ( x, y ) grid of
                    Just cell ->
                        cell

                    Nothing ->
                        default
        in
        Grid.rectangle width height filler


setAtGridCoordinate : Coordinate -> Int -> Layer -> Layer
setAtGridCoordinate coord value (Layer inner) =
    if value >= inner.min && value <= inner.max then
        Layer
            { inner
                | grid = inner.grid |> Grid.set coord value
            }

    else
        Layer inner



-- HELPERS


defaultColor : Color
defaultColor =
    Color.blue


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
applyFloorToColorStops : Int -> DiscreteGradient -> DiscreteGradient
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
applyCeilingToColorStops : Int -> DiscreteGradient -> DiscreteGradient
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
removeColorStops : List ColorStop -> DiscreteGradient -> DiscreteGradient
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
