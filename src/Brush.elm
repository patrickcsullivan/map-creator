module Brush exposing (Brush, Shape(..), makeBrush, width)

import Grid exposing (Coordinate, Grid)


type alias Brush =
    { mask : Grid Bool
    , centerOffset : { x : Int, y : Int }
    , paintValue : Int
    }


type Shape
    = Circle
    | Square


{-| Create a brush with the given shape, width, and paint value. Note that only brushes with odd widths will have an
accurate center offset since a brush with an even width does not actually have a true center cell.
-}
makeBrush : Shape -> Int -> Int -> Brush
makeBrush shape width_ paintValue =
    let
        mask =
            case shape of
                Circle ->
                    circleMask width_

                Square ->
                    squareMask width_
    in
    { mask = mask
    , centerOffset = { x = width_ // 2, y = width_ // 2 }
    , paintValue = paintValue
    }


{-| Get the brush width.
-}
width : Brush -> Int
width brush =
    Grid.width brush.mask


{-| Create a square grid of Boolean values that create a circular mask. Cells containing True are masked cells, and
cells containing False are unmasked cells.
-}
circleMask : Int -> Grid Bool
circleMask brushWidth =
    Grid.square brushWidth (isInCircleMask brushWidth)


{-| Create a square grid of Boolean values that create a square mask. All cells contain True, indicating that all cells
are masked cells.
-}
squareMask : Int -> Grid Bool
squareMask brushWidth =
    Grid.square brushWidth (\_ _ -> True)


{-| Check if a cell is in a circular mask on a square grid. A cell is in the mask if the corner of the cell that is
closest to the circle center is within the circle.
-}
isInCircleMask : Int -> Int -> Int -> Bool
isInCircleMask maskWidth x y =
    let
        radius =
            toFloat maskWidth / 2.0

        centerPoint =
            ( radius, radius )

        closestCornerToCenter =
            closestCorner centerPoint ( x, y )
    in
    closestCornerToCenter
        |> isInCircle centerPoint radius


{-| Check if a point is inside a circle.
-}
isInCircle : ( Float, Float ) -> Float -> ( Float, Float ) -> Bool
isInCircle center radius point =
    sqrDist center point <= radius * radius


{-| Squared distance between two points.
-}
sqrDist : ( Float, Float ) -> ( Float, Float ) -> Float
sqrDist ( x, y ) ( x2, y2 ) =
    (x2 - x) * (x2 - x) + (y2 - y) * (y2 - y)


{-| Get the floating point coordinates of the cell corner that is closest to
the given floating point coordinates. The cell is identifed by its x and y
indices. A cell at index (x, y) has corners at (x, y), (x+1, y), (x, y+1), and
(x+1, y+1).
-}
closestCorner : ( Float, Float ) -> ( Int, Int ) -> ( Float, Float )
closestCorner ( targetX, targetY ) ( cellX, cellY ) =
    let
        cellCenterX =
            toFloat cellX + 0.5

        cellCenterY =
            toFloat cellY + 0.5

        x =
            if targetX <= cellCenterX then
                toFloat cellX

            else
                toFloat (cellX + 1)

        y =
            if targetY <= cellCenterY then
                toFloat cellY

            else
                toFloat (cellY + 1)
    in
    ( x, y )
