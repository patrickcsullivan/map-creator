module GridEditor exposing (Msg, State, init, update, updateGrid, updatePaneSize, view)

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
        g =
            boundGrid model.cellMin model.cellMax grid
    in
    State
        { model
            | grid = g
        }
        |> Debug.log "State"


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
            |> gridView
            |> Collage.Render.svg
        ]


gridView : Grid Int -> Collage Msg
gridView rows =
    rows
        |> Array.toList
        |> List.map rowView
        |> Collage.Layout.vertical


rowView : Array Int -> Collage Msg
rowView cells =
    cells
        |> Array.toList
        |> List.map cellView
        |> Collage.Layout.horizontal


cellView : Int -> Collage Msg
cellView _ =
    let
        color =
            Color.blue

        fill =
            Collage.uniform color

        border =
            Collage.solid 1.5 <| Collage.uniform <| darken color
    in
    Collage.square 30.5
        |> Collage.styled ( fill, border )


darken : Color -> Color
darken color =
    let
        c =
            Color.toHsla color
    in
    Color.hsla
        c.hue
        c.saturation
        (max 0 <| c.lightness - 0.125)
        c.alpha



-- HELPERS


boundGrid : Int -> Int -> Grid Int -> Grid Int
boundGrid lower upper =
    Grid.map (boundInt lower upper)


boundInt : Int -> Int -> Int -> Int
boundInt lower upper =
    max lower << min upper
