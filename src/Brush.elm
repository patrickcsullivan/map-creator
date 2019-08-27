module Brush exposing (Brush, Effect, Shape, getMask)

import Grid exposing (Coordinate, Grid)


type Brush
    = Brush InnerBrush


type alias InnerBrush =
    { shape : Shape
    , effect : Effect
    , width : Int
    , value : Int
    }


type Shape
    = Circle
    | Square


type Effect
    = Absolute
    | Additive


{-| Get a mask indicating which coordinates on a canvas the brush is on.
-}
getMask : Brush -> Coordinate -> Int -> Int -> Grid Bool
getMask (Brush inner) brushCoord gridWidth gridHeight =
    case inner.shape of
        Circle ->
            circleMask brushCoord inner.width gridWidth gridHeight

        Square ->
            squareMask brushCoord inner.width gridWidth gridHeight



-- UNEXPORTED


circleMask : Coordinate -> Int -> Int -> Int -> Grid Bool
circleMask brushCoord brushWidth gridWidth gridHeight =
    Grid.rectangle
        gridWidth
        gridHeight
        (isInCircleMask brushCoord brushWidth)


squareMask : Coordinate -> Int -> Int -> Int -> Grid Bool
squareMask brushCoord brushWidth gridWidth gridHeight =
    Grid.rectangle
        gridWidth
        gridHeight
        (isInSquareMask brushCoord brushWidth)


{-| Check if a cell is in the square mask.
-}
isInSquareMask : ( Int, Int ) -> Int -> Int -> Int -> Bool
isInSquareMask ( maskCenterX, maskCenterY ) maskWidth x y =
    if isOdd maskWidth then
        let
            inMaskX =
                abs (maskCenterX - x) <= (maskWidth - 1) // 2

            inMaskY =
                abs (maskCenterY - y) <= (maskWidth - 1) // 2
        in
        inMaskX && inMaskY

    else
        let
            inMaskX =
                abs (maskCenterX - x) <= maskWidth // 2

            inMaskY =
                abs (maskCenterY - y) <= maskWidth // 2
        in
        inMaskX && inMaskY


{-| Check if a cell is in the circular mask.
-}
isInCircleMask : ( Int, Int ) -> Int -> Int -> Int -> Bool
isInCircleMask maskCenter maskWidth x y =
    let
        radius =
            toFloat maskWidth / 2.0

        centerPoint =
            cellIndexToCenterPoint maskCenter

        closestCornerPoint =
            closestCorner centerPoint ( x, y )
    in
    closestCornerPoint
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


{-| Get the center point for a cell at the given index. Cells are squares with
a width of 1, and a cell with the index (x, y) has a center at (x+0.5, y+0.5).
-}
cellIndexToCenterPoint : ( Int, Int ) -> ( Float, Float )
cellIndexToCenterPoint ( xIndex, yIndex ) =
    ( toFloat xIndex + 0.5, toFloat yIndex + 0.5 )


isEven : Int -> Bool
isEven x =
    modBy x 2 == 0


isOdd : Int -> Bool
isOdd =
    not << isEven
