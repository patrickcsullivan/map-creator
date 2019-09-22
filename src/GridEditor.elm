module GridEditor exposing
    ( Msg
    , State
    , init
    ,  update
       -- , updateGradient
       -- , updateGrid

    , updateGradient
    , updateGrid
    , updateWithElapsedTime
    , view
    )

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
    , gradient : DiscreteGradient
    , currentTime : Float
    , width : Float
    , height : Float
    , cameraCenter : ( Float, Float )
    , activeAction : Maybe ActiveAction
    , zoomStep : Float
    , zoomStepDestination : Float
    }


type ActiveAction
    = PanAction PanState


type alias PanState =
    { lastCameraCenter : ( Float, Float )
    , lastMousePosition : ( Float, Float )
    }


init : Grid Int -> DiscreteGradient -> Float -> Float -> State
init grid gradient canvasWidth canvasHeight =
    -- let
    --     g =
    --         boundGrid cellMin cellMax grid
    -- in
    State
        { grid = grid
        , gradient = gradient
        , currentTime = 0
        , width = canvasWidth
        , height = canvasHeight
        , cameraCenter = ( 0, 0 )
        , activeAction = Nothing
        , zoomStep = 0
        , zoomStepDestination = 0
        }



-- TODO: Rename to makePanAction


initPanAction : ( Float, Float ) -> ( Float, Float ) -> ActiveAction
initPanAction cameraCenter mousePosition =
    PanAction
        { lastCameraCenter = cameraCenter
        , lastMousePosition = mousePosition
        }



--------------------------------------------------------------------------------
-- UPDATE


type Msg
    = Resize Int Int
      -- | MouseMove { x : Float, y : Float }
    | Scroll Float
      -- Experimental
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

        -- MouseMove pos ->
        --     ( { model
        --         | mousePosition = Just ( pos.x, pos.y )
        --       }
        --     , Cmd.none
        --     )
        Scroll dy ->
            { model
              -- A dy of 50 corresponds to a full "step".
                | zoomStepDestination = model.zoomStepDestination + (dy / 50.0)
            }

        MouseDown button pos ->
            (case button of
                Mouse.MiddleButton ->
                    { model
                        | activeAction = Just <| initPanAction model.cameraCenter pos
                    }

                _ ->
                    model
            )
                |> Debug.log "MouseDown"

        MouseUp ->
            model |> stopAction

        MouseMove pos ->
            (case model.activeAction of
                Just (PanAction panState) ->
                    let
                        bounds =
                            cameraBounds model.cameraCenter model.width model.height model.zoomStep

                        lastMousePosInScene =
                            mouseToScenePos bounds model.width model.height panState.lastMousePosition

                        currentMousePosInScene =
                            mouseToScenePos bounds model.width model.height pos

                        deltaX =
                            Tuple.first currentMousePosInScene - Tuple.first lastMousePosInScene

                        deltaY =
                            -- Last and current are flipped because y values increase from top to bottom in mouse
                            -- coordinates but increase from bottom to top in scene coordinates.
                            Tuple.second lastMousePosInScene - Tuple.second currentMousePosInScene

                        newCameraCenter =
                            ( Tuple.first model.cameraCenter - deltaX
                            , Tuple.second model.cameraCenter - deltaY
                            )
                    in
                    { model
                        | cameraCenter = newCameraCenter
                        , activeAction = Just <| initPanAction newCameraCenter pos
                    }

                _ ->
                    model
            )
                |> Debug.log "MouseMove"

        MouseLeave ->
            let
                _ =
                    Debug.log "MouseLeave" ""
            in
            model |> stopAction


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


updateGrid : Grid Int -> State -> State
updateGrid grid (State model) =
    State
        { model
            | grid = grid
        }


updateGradient : DiscreteGradient -> State -> State
updateGradient gradient (State model) =
    State
        { model
            | gradient = gradient
        }


stopAction : Model -> Model
stopAction model =
    { model
        | activeAction = Nothing
    }



-- mousePosition : Decode.Decoder Msg
-- mousePosition =
--     Decode.map2 (\x y -> MouseMove { x = x, y = y })
--         (Decode.field "pageX" Decode.float)
--         (Decode.field "pageY" Decode.float)


scroll : Wheel.Event -> Msg
scroll wheelEvent =
    Scroll wheelEvent.deltaY


mouseToScenePos : CameraBounds -> Float -> Float -> ( Float, Float ) -> ( Float, Float )
mouseToScenePos bounds canvasWidth canvasHeight ( mouseX, mouseY ) =
    let
        cameraWidth =
            bounds.right - bounds.left

        cameraHeight =
            bounds.top - bounds.bottom

        x =
            (mouseX / canvasWidth) * cameraWidth + bounds.left

        y =
            (mouseY / canvasHeight) * cameraHeight + bounds.bottom
    in
    ( x, y )



--------------------------------------------------------------------------------
-- VIEW


view : State -> Html Msg
view (State model) =
    WebGL.toHtml
        [ Html.Attributes.width (round model.width)
        , Html.Attributes.height (round model.height)
        , Html.Attributes.style "display" "block"
        , Wheel.onWheel scroll
        , Mouse.onDown (\event -> MouseDown event.button event.clientPos)
        , Mouse.onUp (\_ -> MouseUp)
        , Mouse.onMove (\event -> MouseMove event.clientPos)
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


{-| Make a 2D orthographic camera from the given left, right, bottom, and top bounds.
-}
cameraFromBounds : CameraBounds -> Mat4
cameraFromBounds bounds =
    Mat4.makeOrtho2D bounds.left bounds.right bounds.bottom bounds.top


{-| Orthographic camera's left, right, bottom, and top bounds.
-}
type alias CameraBounds =
    { left : Float
    , right : Float
    , bottom : Float
    , top : Float
    }


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


trianglesFromGrid : DiscreteGradient -> Grid Int -> List Triangle
trianglesFromGrid gradient grid =
    let
        gridOriginX =
            -1 * cellWidth * (toFloat <| Grid.width grid) * 0.5

        gridOriginY =
            -1 * cellWidth * (toFloat <| Grid.height grid) * 0.5
    in
    grid
        |> Grid.toListWithCoordinates
        |> List.map (\( ( i, j ), cellValue ) -> trianglesFromCell gradient gridOriginX gridOriginY i j cellValue)
        |> List.concat


trianglesFromCell : DiscreteGradient -> Float -> Float -> Int -> Int -> Int -> List Triangle
trianglesFromCell gradient gridOriginX gridOriginY cellIndexI cellIndexJ cellValue =
    let
        cellOriginX =
            gridOriginX + toFloat cellIndexI * cellWidth

        cellOriginY =
            gridOriginY + toFloat cellIndexJ * cellWidth

        color =
            DiscreteGradient.getColorAt cellValue gradient
                |> vec3FromColor
    in
    [ ( Vertex (vec3 cellOriginX cellOriginY 0) color
      , Vertex (vec3 cellOriginX (cellOriginY + cellWidth) 0) color
      , Vertex (vec3 (cellOriginX + cellWidth) (cellOriginY + cellWidth) 0) color
      )
    , ( Vertex (vec3 cellOriginX cellOriginY 0) color
      , Vertex (vec3 (cellOriginX + cellWidth) (cellOriginY + cellWidth) 0) color
      , Vertex (vec3 (cellOriginX + cellWidth) cellOriginY 0) color
      )
    ]


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
