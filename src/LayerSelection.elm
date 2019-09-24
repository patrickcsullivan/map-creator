module LayerSelection exposing
    ( LayerSelection
    , add
    , deleteSelected
    , fromList
    , length
    , map
    , mapSelected
    , select
    , selectedIndex
    , selectedLayer
    , singleton
    , toList
    )

import Layer exposing (Layer)
import List.Nonempty as Nonempty exposing (Nonempty)


{-| Models a non-empty list of layers where one layer is always selected.
-}
type LayerSelection
    = LayerSelection Inner


type alias Inner =
    { list : Nonempty Layer
    , selectedIndex : Int
    }


singleton : Layer -> LayerSelection
singleton layer =
    LayerSelection
        { list = Nonempty.fromElement layer
        , selectedIndex = 0
        }


fromList : List Layer -> Maybe LayerSelection
fromList layers =
    layers
        |> Nonempty.fromList
        |> Maybe.map
            (\nonEmptyList ->
                LayerSelection
                    { list = nonEmptyList
                    , selectedIndex = 0
                    }
            )


length : LayerSelection -> Int
length (LayerSelection inner) =
    Nonempty.length inner.list


map : (Layer -> Layer) -> LayerSelection -> LayerSelection
map f (LayerSelection inner) =
    LayerSelection
        { inner
            | list = Nonempty.map f inner.list
        }


mapSelected : (Layer -> Layer) -> LayerSelection -> LayerSelection
mapSelected f (LayerSelection inner) =
    LayerSelection
        { inner
            | list =
                Nonempty.indexedMap
                    (\i layer ->
                        if i == inner.selectedIndex then
                            f layer

                        else
                            layer
                    )
                    inner.list
        }


toList : LayerSelection -> List Layer
toList (LayerSelection inner) =
    inner.list |> Nonempty.toList


selectedLayer : LayerSelection -> Layer
selectedLayer (LayerSelection inner) =
    Nonempty.get inner.selectedIndex inner.list


selectedIndex : LayerSelection -> Int
selectedIndex (LayerSelection inner) =
    inner.selectedIndex


select : Int -> LayerSelection -> LayerSelection
select index (LayerSelection inner) =
    let
        boundedIndex =
            index
                |> min (Nonempty.length inner.list - 1)
                |> max 0
    in
    LayerSelection
        { inner
            | selectedIndex = boundedIndex
        }


deleteSelected : LayerSelection -> LayerSelection
deleteSelected (LayerSelection inner) =
    let
        unsafeList =
            inner.list |> Nonempty.toList

        unsafeListAfterRemove =
            List.take inner.selectedIndex unsafeList
                ++ List.drop (inner.selectedIndex + 1) unsafeList
    in
    case Nonempty.fromList unsafeListAfterRemove of
        Just nonemptyListAfterRemove ->
            LayerSelection
                { inner
                    | list = nonemptyListAfterRemove
                    , selectedIndex = inner.selectedIndex - 1
                }

        Nothing ->
            LayerSelection inner


add : Layer -> LayerSelection -> LayerSelection
add layer (LayerSelection inner) =
    LayerSelection
        { inner
            | list = Nonempty.append inner.list (Nonempty.fromElement layer)
        }
