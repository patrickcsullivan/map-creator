module GridEditor exposing
    ( Msg
    , State
    , init
    , update
    , updateCellMax
    , updateCellMin
    , updateGradient
    , updateGrid
    , updatePaneSize
    , view
    )

import Array exposing (Array)
import Collage exposing (Collage)
import Collage.Layout
import Collage.Render
import Color exposing (Color)
import DiscreteGradient exposing (DiscreteGradient)
import Grid exposing (Coordinate, Grid)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, classList, disabled, style, type_, value)
import Html.Events exposing (onClick, onInput)



-- MODEL


type State
    = State Model


type alias Model =
    { grid : Grid Int
    , cellMin : Int
    , cellMax : Int
    , gradient : DiscreteGradient
    , paneWidth : Int
    , paneHeight : Int
    }


init : Grid Int -> Int -> Int -> DiscreteGradient -> Int -> Int -> State
init grid cellMin cellMax gradient paneWidth paneHeight =
    let
        g =
            boundGrid cellMin cellMax grid
    in
    State
        { grid = g
        , cellMin = cellMin
        , cellMax = cellMax
        , gradient = gradient
        , paneWidth = paneWidth
        , paneHeight = paneHeight
        }



-- UPDATE


type Msg
    = Click Float Float


update : Msg -> State -> ( State, Grid Int )
update msg (State model) =
    update_ msg model
        |> Tuple.mapFirst State
        |> Debug.log "State"


updatePaneSize : Int -> Int -> State -> State
updatePaneSize paneWidth paneHeight (State model) =
    State
        { model
            | paneWidth = paneWidth
            , paneHeight = paneHeight
        }
        |> Debug.log "State"


updateGrid : Grid Int -> State -> State
updateGrid grid (State model) =
    let
        boundedGrid =
            boundGrid model.cellMin model.cellMax grid
    in
    State
        { model
            | grid = boundedGrid
        }
        |> Debug.log "State"


updateCellMin : Int -> State -> State
updateCellMin cellMin (State model) =
    let
        boundedGrid =
            boundGrid cellMin model.cellMax model.grid
    in
    State
        { model
            | cellMin = cellMin
            , grid = boundedGrid
        }


updateCellMax : Int -> State -> State
updateCellMax cellMax (State model) =
    let
        boundedGrid =
            boundGrid model.cellMin cellMax model.grid
    in
    State
        { model
            | cellMax = cellMax
            , grid = boundedGrid
        }


updateGradient : DiscreteGradient -> State -> State
updateGradient gradient (State model) =
    State
        { model
            | gradient = gradient
        }


update_ : Msg -> Model -> ( Model, Grid Int )
update_ msg model =
    case msg of
        _ ->
            ( model, model.grid )



-- VIEW


view : State -> Html Msg
view (State model) =
    div [ class "grid-editor" ]
        [ model.grid
            |> gridView model.gradient
            |> Collage.Render.svg
        ]


gridView : DiscreteGradient -> Grid Int -> Collage Msg
gridView gradient rows =
    rows
        |> Array.toList
        |> List.map (rowView gradient)
        |> Collage.Layout.vertical


rowView : DiscreteGradient -> Array Int -> Collage Msg
rowView gradient cells =
    cells
        |> Array.toList
        |> List.map (cellView gradient)
        |> Collage.Layout.horizontal


cellView : DiscreteGradient -> Int -> Collage Msg
cellView gradient cellValue =
    let
        color =
            DiscreteGradient.getColorAt cellValue gradient

        fill =
            Collage.uniform color

        border =
            Collage.solid 1.5 <| Collage.uniform <| borderColor color
    in
    Collage.square 30.5
        |> Collage.styled ( fill, border )


borderColor : Color -> Color
borderColor color =
    let
        c =
            Color.toHsla color
    in
    Color.hsla
        c.hue
        (min 1.0 <| c.saturation + 0.125)
        (max 0.0 <| c.lightness - 0.125)
        c.alpha



-- HELPERS


boundGrid : Int -> Int -> Grid Int -> Grid Int
boundGrid lower upper =
    Grid.map (boundInt lower upper)


boundInt : Int -> Int -> Int -> Int
boundInt lower upper =
    max lower << min upper
