module Stroke exposing (Stroke, empty, getMask)

import Brush exposing (Brush, Shape)
import Grid exposing (Coordinate, Grid)


type Stroke
    = Stroke InnerStroke


type alias InnerStroke =
    { mask : Grid Bool
    }


{-| Create an empty stroke on a canvas.
-}
empty : Int -> Int -> Stroke
empty canvasWidth canvasHeight =
    Stroke { mask = Grid.repeat canvasWidth canvasHeight False }


{-| Get a mask indicating which coordinates on a canvas the strock is applied to.
-}
getMask : Stroke -> Grid Bool
getMask (Stroke inner) =
    inner.mask


{-| Add the cells covered by the brush to the stroke.
-}
continue : Brush -> Coordinate -> Stroke -> Stroke
continue brush brushCoord (Stroke inner) =
    let
        w =
            Grid.width inner.mask

        h =
            Grid.width inner.mask

        brushMask =
            Brush.mask brush brushCoord w h

        filler x y =
            let
                onBrushMask =
                    Grid.get ( x, y ) brushMask

                onStrokeMask =
                    Grid.get ( x, y ) inner.mask
            in
            case ( onBrushMask, onStrokeMask ) of
                ( Just True, _ ) ->
                    True

                ( Just _, Just True ) ->
                    True

                _ ->
                    False
    in
    Stroke { mask = Grid.rectangle w h filler }
