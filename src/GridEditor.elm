module GridEditor exposing
    ( Msg
    , State
    , init
    , update
    , updateBrush
    , updateCanvasSize
    , updateCellMax
    , updateCellMin
    , updateGradient
    , updateGrid
    , updateWithElapsedTime
    , view
    )

import Brush exposing (Brush)
import Color exposing (Color)
import DiscreteGradient exposing (DiscreteGradient)
import Grid exposing (Grid)
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as Decode
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import WebGL exposing (Mesh, Shader)



--------------------------------------------------------------------------------
-- MODEL


{-| Camera z position is muplitplied by this factor once for each "step" the camera is zoomed out past default and
divided by this factor once for each "step" the camera is zoomed in past the default.
-}
zoomFactorPerStep : Float
zoomFactorPerStep =
    1.333


{-| Time in seconds to scroll between zoom "steps".
-}
secondsPerZoomStep : Float
secondsPerZoomStep =
    0.1


type State
    = State Model


type alias Model =
    { grid : Grid Int
    , cellMin : Int
    , cellMax : Int
    , gradient : DiscreteGradient
    , brush : Brush
    , mouseAndActiveAction : Maybe MouseAndActiveAction
    , currentTime : Float
    , width : Float
    , height : Float
    , cameraCenter : ( Float, Float )
    , zoomStep : Float
    , zoomStepDestination : Float
    }


type alias MouseAndActiveAction =
    { mousePosition : ( Float, Float )
    , activeAction : Maybe ActiveAction
    }


type ActiveAction
    = PanAction PanHistory
    | PaintAction


type alias PanHistory =
    { lastMousePosition : ( Float, Float )
    }


init : Grid Int -> Int -> Int -> DiscreteGradient -> Brush -> Float -> Float -> State
init grid cellMin cellMax gradient brush canvasWidth canvasHeight =
    let
        g =
            boundGrid cellMin cellMax grid
    in
    State
        { grid = g
        , cellMin = cellMin
        , cellMax = cellMax
        , gradient = gradient
        , brush = brush
        , mouseAndActiveAction = Nothing
        , currentTime = 0
        , width = canvasWidth
        , height = canvasHeight
        , cameraCenter = ( 0, 0 )
        , zoomStep = 0
        , zoomStepDestination = 0
        }



--------------------------------------------------------------------------------
-- UPDATE


type Msg
    = Resize Int Int
    | Scroll Float
    | MouseDown Mouse.Button ( Float, Float )
    | MouseUp
    | MouseMove ( Float, Float )
    | MouseLeave


update : Msg -> State -> ( State, Grid Int )
update msg (State model) =
    ( State <| update_ msg model, model.grid )



-- |> Tuple.mapFirst State


update_ : Msg -> Model -> Model
update_ msg model =
    case msg of
        Resize w h ->
            { model
                | width = toFloat w
                , height = toFloat h
            }

        Scroll dy ->
            { model
              -- A dy of 50 corresponds to a full "step".
                | zoomStepDestination = model.zoomStepDestination + (dy / 50.0)
            }

        MouseDown button pos ->
            let
                newActiveAction =
                    case button of
                        Mouse.MiddleButton ->
                            Just <| PanAction { lastMousePosition = pos }

                        Mouse.MainButton ->
                            Just PaintAction

                        _ ->
                            Nothing
            in
            model
                |> updateMousePosition pos
                |> updateActiveAction newActiveAction
                |> applyActiveAction

        MouseUp ->
            updateActiveAction Nothing model

        MouseMove pos ->
            model
                |> updateMousePosition pos
                |> applyActiveAction

        MouseLeave ->
            updateActiveAction Nothing model


updateActiveAction : Maybe ActiveAction -> Model -> Model
updateActiveAction activeAction model =
    { model
        | mouseAndActiveAction =
            model.mouseAndActiveAction
                |> Maybe.map
                    (\ma ->
                        { ma
                            | activeAction = activeAction
                        }
                    )
    }


updateMousePosition : ( Float, Float ) -> Model -> Model
updateMousePosition pos model =
    let
        newMouseAndActiveAction =
            case model.mouseAndActiveAction of
                Just ma ->
                    { ma
                        | mousePosition = pos
                    }

                Nothing ->
                    { mousePosition = pos
                    , activeAction = Nothing
                    }
    in
    { model
        | mouseAndActiveAction = Just newMouseAndActiveAction
    }


applyActiveAction : Model -> Model
applyActiveAction model =
    case model.mouseAndActiveAction of
        Just ma ->
            case ma.activeAction of
                Just (PanAction panHistory) ->
                    let
                        ( newCameraCenter, updatedPanHistory ) =
                            applyPanAction model.cameraCenter model.width model.height model.zoomStep panHistory ma.mousePosition
                    in
                    { model
                        | cameraCenter = newCameraCenter
                        , mouseAndActiveAction = Just { ma | activeAction = Just (PanAction updatedPanHistory) }
                    }

                Just PaintAction ->
                    let
                        updatedGrid =
                            applyPaintAction model.cameraCenter model.width model.height model.zoomStep model.cellMin model.cellMax model.grid model.brush ma.mousePosition
                    in
                    { model
                        | grid = updatedGrid
                    }

                Nothing ->
                    model

        -- TODO: Finish
        Nothing ->
            model


{-| Pan the camera along the vector between the previous mouse position and the current mouse position.
-}
applyPanAction : ( Float, Float ) -> Float -> Float -> Float -> PanHistory -> ( Float, Float ) -> ( ( Float, Float ), PanHistory )
applyPanAction cameraCenter canvasWidth canvasHeight zoomStep panHistory mousePos =
    let
        bounds =
            cameraBounds cameraCenter canvasWidth canvasHeight zoomStep

        lastMousePosInScene =
            mouseToScenePos bounds canvasWidth canvasHeight panHistory.lastMousePosition

        currentMousePosInScene =
            mouseToScenePos bounds canvasWidth canvasHeight mousePos

        deltaX =
            Tuple.first currentMousePosInScene - Tuple.first lastMousePosInScene

        deltaY =
            Tuple.second currentMousePosInScene - Tuple.second lastMousePosInScene

        newCameraCenter =
            ( Tuple.first cameraCenter - deltaX
            , Tuple.second cameraCenter - deltaY
            )
    in
    ( newCameraCenter
    , { lastMousePosition = mousePos }
    )


applyPaintAction : ( Float, Float ) -> Float -> Float -> Float -> Int -> Int -> Grid Int -> Brush -> ( Float, Float ) -> Grid Int
applyPaintAction cameraCenter canvasWidth canvasHeight zoomStep cellMin cellMax grid brush mousePos =
    let
        bounds =
            cameraBounds cameraCenter canvasWidth canvasHeight zoomStep

        brushCenter =
            mousePos
                |> mouseToScenePos bounds canvasWidth canvasHeight
                |> scenePosToCellIndex grid
    in
    case brushCenter of
        Just bc ->
            grid
                |> applyBrushMask brush bc
                |> maskedMap (\_ -> brush.paintValue)
                |> removeMask
                |> boundGrid cellMin cellMax

        Nothing ->
            grid


updateWithElapsedTime : Float -> State -> State
updateWithElapsedTime t (State model) =
    State
        { model
            | currentTime = model.currentTime + t
            , zoomStep =
                if model.zoomStep == model.zoomStepDestination then
                    model.zoomStep

                else
                    let
                        dStep =
                            t / 1000.0 / secondsPerZoomStep
                    in
                    if model.zoomStep < model.zoomStepDestination then
                        min
                            model.zoomStepDestination
                            (model.zoomStep + dStep)

                    else
                        max
                            model.zoomStepDestination
                            (model.zoomStep - dStep)
        }


updateCanvasSize : Float -> Float -> State -> State
updateCanvasSize w h (State model) =
    State
        { model
            | width = w
            , height = h
        }


updateCellMin : Int -> State -> State
updateCellMin cellMin (State model) =
    State
        { model
            | cellMin = cellMin
            , grid = boundGrid cellMin model.cellMax model.grid
        }


updateCellMax : Int -> State -> State
updateCellMax cellMax (State model) =
    State
        { model
            | cellMax = cellMax
            , grid = boundGrid model.cellMin cellMax model.grid
        }


updateGrid : Grid Int -> State -> State
updateGrid grid (State model) =
    State
        { model
            | grid = boundGrid model.cellMin model.cellMax grid
        }


updateGradient : DiscreteGradient -> State -> State
updateGradient gradient (State model) =
    State
        { model
            | gradient = gradient
        }


updateBrush : Brush -> State -> State
updateBrush brush (State model) =
    State
        { model
            | brush = brush
        }



--------------------------------------------------------------------------------
-- BRUSH


applyBrushMask : Brush -> ( Int, Int ) -> Grid a -> Grid ( a, Bool )
applyBrushMask brush ( brushCenterX, brushCenterY ) =
    Grid.mapWithCoordinate
        (\( x, y ) cell ->
            let
                xRelativeToMask =
                    x - brushCenterX + brush.centerOffset.x

                yRelativeToMask =
                    y - brushCenterY + brush.centerOffset.y

                isInMask =
                    case Grid.get ( xRelativeToMask, yRelativeToMask ) brush.mask of
                        Just True ->
                            True

                        _ ->
                            False
            in
            ( cell, isInMask )
        )


maskedMap : (a -> a) -> Grid ( a, Bool ) -> Grid ( a, Bool )
maskedMap f =
    Grid.map
        (\( cell, isMasked ) ->
            if isMasked then
                ( f cell, isMasked )

            else
                ( cell, isMasked )
        )


removeMask : Grid ( a, Bool ) -> Grid a
removeMask =
    Grid.map (\( cell, _ ) -> cell)



--------------------------------------------------------------------------------
-- CAMERA, SCENE, AND POSITIONING LOGIC


{-| Orthographic camera's left, right, bottom, and top bounds.
-}
type alias CameraBounds =
    { left : Float
    , right : Float
    , bottom : Float
    , top : Float
    }


{-| Make a 2D orthographic camera from the given left, right, bottom, and top bounds.
-}
cameraFromBounds : CameraBounds -> Mat4
cameraFromBounds bounds =
    Mat4.makeOrtho2D bounds.left bounds.right bounds.bottom bounds.top


{-| Calculate the orthographic camera's left, right, bottom, and top bounds.
-}
cameraBounds : ( Float, Float ) -> Float -> Float -> Float -> CameraBounds
cameraBounds ( cameraCenterX, cameraCenterY ) canvasWidth canvasHeight zoomStep =
    let
        -- When zoom "step" is at 0, a 250 px wide canvas should have an ortho camera with a width of 1.0.
        pixelsPerSceneUnit =
            250.0

        -- Factor by which to increase or decrease window size to simulate zoom out or zoom in.
        zoomFactor =
            zoomFactorPerStep ^ zoomStep

        cameraWidth =
            canvasWidth / pixelsPerSceneUnit * zoomFactor

        cameraHeight =
            canvasHeight / pixelsPerSceneUnit * zoomFactor
    in
    { left = -1 * cameraWidth / 2.0 + cameraCenterX
    , right = cameraWidth / 2.0 + cameraCenterX
    , bottom = -1 * cameraHeight / 2.0 + cameraCenterY
    , top = cameraHeight / 2.0 + cameraCenterY
    }


{-| Convert a mouse position in the canvas to an (x, y) position in the scene. Mouse position is relative to the top-
left corner of the canvas.
-}
mouseToScenePos : CameraBounds -> Float -> Float -> ( Float, Float ) -> ( Float, Float )
mouseToScenePos bounds canvasWidth canvasHeight ( mouseX, mouseY ) =
    let
        cameraWidth =
            bounds.right - bounds.left

        cameraHeight =
            bounds.top - bounds.bottom

        x =
            bounds.left + (mouseX / canvasWidth) * cameraWidth

        y =
            bounds.top - (mouseY / canvasHeight) * cameraHeight
    in
    ( x, y )


{-| Convert an (x, y) position in the scene to a cell index on the grid.
-}
scenePosToCellIndex : Grid Int -> ( Float, Float ) -> Maybe ( Int, Int )
scenePosToCellIndex grid ( x, y ) =
    let
        columnCount =
            Grid.width grid

        rowCount =
            Grid.height grid

        gridLeftX =
            gridLeftInScene <| columnCount

        gridBottomY =
            gridBottomInScene <| rowCount

        i =
            Basics.floor <| (x - gridLeftX) / cellWidth

        j =
            Basics.floor <| (y - gridBottomY) / cellWidth
    in
    if (i >= 0) && (i < columnCount) && (j >= 0) && (j < rowCount) then
        Just ( i, j )

    else
        Nothing



--------------------------------------------------------------------------------
-- VIEW


view : State -> Html Msg
view (State model) =
    WebGL.toHtml
        [ Html.Attributes.width (round model.width)
        , Html.Attributes.height (round model.height)
        , Html.Attributes.style "display" "block"
        , Wheel.onWheel (\event -> Scroll event.deltaY)
        , Mouse.onDown (\event -> MouseDown event.button event.offsetPos)
        , Mouse.onUp (\_ -> MouseUp)
        , Mouse.onMove (\event -> MouseMove event.offsetPos)
        , Mouse.onLeave (\_ -> MouseLeave)
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            (mesh model.gradient model.grid)
            { camera =
                cameraBounds model.cameraCenter model.width model.height model.zoomStep
                    |> cameraFromBounds
            }
        ]



--------------------------------------------------------------------------------
-- MESH


type alias Triangle =
    ( Vertex, Vertex, Vertex )


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : DiscreteGradient -> Grid Int -> Mesh Vertex
mesh gradient grid =
    trianglesFromGrid gradient grid
        |> WebGL.triangles


cellWidth : Float
cellWidth =
    0.1


borderInset : Float
borderInset =
    0.005


{-| Get the x position in the scene of the left side of a grid that has the given number of columns.
-}
gridLeftInScene : Int -> Float
gridLeftInScene columnCount =
    -1 * cellWidth * toFloat columnCount * 0.5


{-| Get the y position in the scene of the right side of a grid that has the given number of columns.
-}
gridBottomInScene : Int -> Float
gridBottomInScene rowCount =
    -1 * cellWidth * toFloat rowCount * 0.5


{-| Create the list of triangles needed to render the grid.
-}
trianglesFromGrid : DiscreteGradient -> Grid Int -> List Triangle
trianglesFromGrid gradient grid =
    let
        gridOriginX =
            gridLeftInScene <| Grid.width grid

        gridOriginY =
            gridBottomInScene <| Grid.height grid
    in
    grid
        |> Grid.toListWithCoordinates
        |> List.map (\( ( i, j ), cellValue ) -> trianglesFromCell gradient gridOriginX gridOriginY i j cellValue)
        |> List.concat


{-| Create the list of triangles needed to render a grid cell.
-}
trianglesFromCell : DiscreteGradient -> Float -> Float -> Int -> Int -> Int -> List Triangle
trianglesFromCell gradient gridOriginX gridOriginY cellIndexI cellIndexJ cellValue =
    let
        cellLeft =
            gridOriginX + toFloat cellIndexI * cellWidth

        cellRight =
            cellLeft + cellWidth

        cellBottom =
            gridOriginY + toFloat cellIndexJ * cellWidth

        cellTop =
            cellBottom + cellWidth

        insetLeft =
            cellLeft + borderInset

        insetRight =
            cellRight - borderInset

        insetBottom =
            cellBottom + borderInset

        insetTop =
            cellTop - borderInset

        colorFromGradient =
            DiscreteGradient.getColorAt cellValue gradient

        color =
            colorFromGradient
                |> vec3FromColor

        borderColor =
            colorFromGradient
                |> makeBorderColor
                |> vec3FromColor

        leftBorder =
            [ ( Vertex (vec3 cellLeft cellTop 0) borderColor
              , Vertex (vec3 insetLeft insetTop 0) borderColor
              , Vertex (vec3 cellLeft cellBottom 0) borderColor
              )
            , ( Vertex (vec3 insetLeft insetTop 0) borderColor
              , Vertex (vec3 insetLeft insetBottom 0) borderColor
              , Vertex (vec3 cellLeft cellBottom 0) borderColor
              )
            ]

        rightBorder =
            [ ( Vertex (vec3 insetRight insetTop 0) borderColor
              , Vertex (vec3 cellRight cellTop 0) borderColor
              , Vertex (vec3 cellRight cellBottom 0) borderColor
              )
            , ( Vertex (vec3 insetRight insetTop 0) borderColor
              , Vertex (vec3 cellRight cellBottom 0) borderColor
              , Vertex (vec3 insetRight insetBottom 0) borderColor
              )
            ]

        topBorder =
            [ ( Vertex (vec3 cellLeft cellTop 0) borderColor
              , Vertex (vec3 cellRight cellTop 0) borderColor
              , Vertex (vec3 insetRight insetTop 0) borderColor
              )
            , ( Vertex (vec3 cellLeft cellTop 0) borderColor
              , Vertex (vec3 insetRight insetTop 0) borderColor
              , Vertex (vec3 insetLeft insetTop 0) borderColor
              )
            ]

        bottomBorder =
            [ ( Vertex (vec3 insetLeft insetBottom 0) borderColor
              , Vertex (vec3 insetRight insetBottom 0) borderColor
              , Vertex (vec3 cellRight cellBottom 0) borderColor
              )
            , ( Vertex (vec3 insetLeft insetBottom 0) borderColor
              , Vertex (vec3 cellRight cellBottom 0) borderColor
              , Vertex (vec3 cellLeft cellBottom 0) borderColor
              )
            ]

        innerCell =
            [ ( Vertex (vec3 cellLeft cellTop 0) color
              , Vertex (vec3 cellRight cellTop 0) color
              , Vertex (vec3 cellRight cellBottom 0) color
              )
            , ( Vertex (vec3 cellLeft cellTop 0) color
              , Vertex (vec3 cellRight cellBottom 0) color
              , Vertex (vec3 cellLeft cellBottom 0) color
              )
            ]
    in
    [ leftBorder
    , rightBorder
    , topBorder
    , bottomBorder
    , innerCell
    ]
        |> List.concat


makeBorderColor : Color -> Color
makeBorderColor color =
    let
        c =
            Color.toHsla color
    in
    Color.hsla
        c.hue
        (min 1.0 <| c.saturation + 0.125)
        (max 0.0 <| c.lightness - 0.125)
        c.alpha


vec3FromColor : Color -> Vec3
vec3FromColor color =
    let
        rbga =
            Color.toRgba color
    in
    vec3 rbga.red rbga.green rbga.blue



--------------------------------------------------------------------------------
-- SHADERS


type alias Uniforms =
    { camera : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 camera;
        varying vec3 vcolor;

        void main () {
            gl_Position = camera * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]



--------------------------------------------------------------------------------
-- HELPERS


boundGrid : Int -> Int -> Grid Int -> Grid Int
boundGrid lower upper =
    Grid.map (boundInt lower upper)


boundInt : Int -> Int -> Int -> Int
boundInt lower upper =
    max lower << min upper
