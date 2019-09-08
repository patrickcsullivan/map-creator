module Update exposing
    ( Msg(..)
    , toSelectLayerMsg
    , toSetLayerMaxMsg
    , toSetLayerMinMsg
    , update
    )

import Model exposing (Model)


type Msg
    = SelectLayer (Maybe Int)
    | DeleteSelectedLayer
    | OpenNewLayerDialog
    | SetNewLayerDialogNameField String
    | NewLayerDialogCancel
    | NewLayerDialogCreate
    | SetLayerMin (Maybe Int)
    | SetLayerMax (Maybe Int)


update : Msg -> Model -> Model
update msg model =
    (case msg |> Debug.log "msg" of
        SelectLayer index ->
            Model.selectLayer index model

        DeleteSelectedLayer ->
            Model.deleteSelectedLayer model

        OpenNewLayerDialog ->
            Model.openNewLayerDialog model

        SetNewLayerDialogNameField layerName ->
            Model.setNewLayerDialogName layerName model

        NewLayerDialogCancel ->
            Model.closeModal model

        NewLayerDialogCreate ->
            case Model.getModal model of
                Just (Model.NewLayerDialog layerName) ->
                    model
                        |> Model.createLayer layerName
                        |> Model.selectLayer (Just (getLayerCount model))
                        |> Model.closeModal

                _ ->
                    model

        SetLayerMin newMin ->
            case newMin of
                Just m ->
                    Model.setSelectedLayerMin m model

                Nothing ->
                    model

        SetLayerMax newMax ->
            case newMax of
                Just m ->
                    Model.setSelectedLayerMax m model

                Nothing ->
                    model
    )
        |> Debug.log "Model"


getLayerCount : Model -> Int
getLayerCount model =
    model
        |> Model.getLayers
        |> List.length



-- MESSAGE BUILDING HELPERS


toSelectLayerMsg : String -> Msg
toSelectLayerMsg =
    SelectLayer << String.toInt


toSetLayerMinMsg : String -> Msg
toSetLayerMinMsg =
    SetLayerMin << String.toInt


toSetLayerMaxMsg : String -> Msg
toSetLayerMaxMsg =
    SetLayerMax << String.toInt
