module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model exposing (Model)


main =
    Browser.sandbox
        { init = Model.init
        , update = update
        , view = view
        }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ toolbar model
        ]


toolbar : Model -> Html Msg
toolbar model =
    div [ class "toolbar" ]
        [ toolbarSectionHeader "Layers"
        ]


toolbarRow : List (Html Msg) -> Html Msg
toolbarRow contents =
    div [ class "toolbar__row" ]
        contents


toolbarSectionHeader : String -> Html Msg
toolbarSectionHeader header =
    div [ class "toolbar__section-header" ]
        [ text header
        ]
