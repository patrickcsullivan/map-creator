module Grid exposing
    ( Grid, Coordinate
    , rectangle, square, repeat, repeatSquare
    , set, get, row, column
    , toColumn, toRow, toCoordinate
    , decoder, height, map, mapWithCoordinate, toJson, toListWithCoordinates, width
    )

{-| Copied from <https://github.com/jreut/elm-grid/blob/1.0.2/src/Grid.elm> with
some small changes and additions

This library provides a data type to represent two-dimensional arrays.


# Definition

@docs Grid, Coordinate


# Creating `Grid`s

@docs rectangle, square, repeat, repeatSquare


# Get and set

@docs set, get, row, column


# Coordinates

@docs toColumn, toRow, toCoordinate

-}

import Array exposing (Array)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (andThen)
import Maybe.Extra exposing (combine)


{-| Data type representing a two-dimensional array. Use this to declare types
in your program. For example, if you wanted to have a `Grid` of Integers, you
could write this:

    type alias MyType =
        Grid Int

-}
type alias Grid a =
    Array (Array a)


{-| Type to use when indexing into `Grid`.
-}
type alias Coordinate =
    ( Int, Int )


{-| Convert a pair of numbers to a coordinate. Column index goes first, then
the row index.
-}
toCoordinate : Int -> Int -> Coordinate
toCoordinate x y =
    ( x, y )


{-| Fetch the column at the given index.

    column 2 (square 3 (+)) == Array.fromList [ 2, 3, 4 ]

    column 2 (square 8 (^)) == Array.fromList [ 1, 2, 4, 8, 16, 32, 64, 128 ]

-}
column : Int -> Grid a -> Maybe (Array a)
column index grid =
    let
        got =
            Array.map (Array.get index) grid
    in
    got
        |> Array.toList
        |> combine
        |> Maybe.map Array.fromList


{-| Fetch the row at the given index.

    row 3 (repeat 1 4 "bun") == Just (Array.fromList [ "bun" ])

-}
row : Int -> Grid a -> Maybe (Array a)
row index grid =
    Array.get index grid


type alias Filler a =
    Int -> Int -> a


{-| Create a grid `w` units by `h` units, filling each cell according
to the cell's coordinate.

    get ( 2, 1 ) (rectangle 4 2 (+)) == Just 3

-}
rectangle : Int -> Int -> Filler a -> Grid a
rectangle w h filler =
    Array.initialize h (\y -> Array.initialize w (\x -> filler x y))


{-| Like `rectangle`, except always make a square grid
-}
square : Int -> Filler a -> Grid a
square size filler =
    rectangle size size filler


{-| Create a grid just like [`Grid#rectangle`](Grid#rectangle), except just
copy a value into each cell.

    get ( 2, 1 ) (rectangle 4 2 "foo") == Just "foo"

-}
repeat : Int -> Int -> a -> Grid a
repeat x y occupant =
    rectangle x y (always << always occupant)


{-| Like `repeat`, except make a square grid.
-}
repeatSquare : Int -> a -> Grid a
repeatSquare size occupant =
    square size (always << always occupant)


coordinate : Int -> Int -> Coordinate
coordinate x y =
    ( x, y )


{-| Fetch the column index from a `Coordinate`. Useful with `column`
-}
toColumn : Coordinate -> Int
toColumn =
    Tuple.first


{-| Fetch the row index from a `Coordinate`. Useful with `row`.
-}
toRow : Coordinate -> Int
toRow =
    Tuple.second


width : Grid a -> Int
width grid =
    Array.get 0 grid
        |> Maybe.map Array.length
        |> Maybe.withDefault 0


height : Grid a -> Int
height grid =
    Array.length grid


{-| Fetch the occupant at a given `Coordinate`.

    get ( 2, 4 ) (square 6 (*)) == Just 8

-}
get : Coordinate -> Grid a -> Maybe a
get coord grid =
    Array.get (toRow coord) grid
        |> andThen (Array.get (toColumn coord))


{-| Overwrite the occupant at a given `Coordinate`
-}
set : Coordinate -> a -> Grid a -> Grid a
set coord occupant grid =
    row (toRow coord) grid
        |> Maybe.map (Array.set (toColumn coord) occupant)
        |> Maybe.map (\r -> Array.set (toRow coord) r grid)
        |> Maybe.withDefault grid


map : (a -> b) -> Grid a -> Grid b
map f grid =
    Array.map (Array.map f) grid


mapWithCoordinate : (Coordinate -> a -> b) -> Grid a -> Grid b
mapWithCoordinate f grid =
    Array.indexedMap
        (\y -> Array.indexedMap (\x -> f (coordinate x y)))
        grid


toListWithCoordinates : Grid a -> List ( ( Int, Int ), a )
toListWithCoordinates grid =
    grid
        |> mapWithCoordinate (\coord cell -> ( coord, cell ))
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat


toJson : (a -> Encode.Value) -> Grid a -> Encode.Value
toJson encodeCell =
    Encode.array (Encode.array encodeCell)


decoder : Decode.Decoder a -> Decode.Decoder (Grid a)
decoder cellDecoder =
    Decode.array (Decode.array cellDecoder)
